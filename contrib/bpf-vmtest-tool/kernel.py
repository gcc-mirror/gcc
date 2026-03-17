import logging
import os
import shutil
import subprocess
from pathlib import Path
import re
import sys
from urllib.parse import urljoin
from urllib.request import urlretrieve
from typing import Optional, List
from dataclasses import dataclass

import config
import utils

logger = logging.getLogger(__name__)


@dataclass
class KernelSpec:
    """Immutable kernel specification"""

    version: str
    arch: str | None = None

    def __post_init__(self):
        if self.arch is None or self.arch == "":
            self.arch = config.config.arch
        self.major = self.version.split(".")[0]

    def __str__(self):
        return f"{self.version}-{self.arch}"

    @property
    def kernel_build_dir(self) -> Path:
        return config.config.kernels_dir / f"linux-{self}-build"

    @property
    def kernel_dir(self) -> Path:
        return config.config.kernels_dir / f"linux-{self}"

    @property
    def bzimage_path(self) -> Path:
        return self.kernel_dir / f"bzImage-{self}"

    @property
    def bpftool_path(self) -> Path:
        return self.kernel_dir / "bpftool"

    @property
    def libbpf_path(self) -> Path:
        return self.kernel_dir / "libbpf.a"

    @property
    def vmlinux_path(self) -> Path:
        return self.kernel_dir / "vmlinux.h"

    @property
    def tarball_path(self) -> Path:
        return config.config.kernels_dir / f"linux-{self}.tar.xz"


class KernelImage:
    """Represents a compiled kernel image"""

    def __init__(self, path: Path):
        if not isinstance(path, Path):
            path = Path(path)

        if not path.exists():
            raise FileNotFoundError(f"Kernel image not found: {path}")

        self.path = path

    def __str__(self):
        return str(self.path)


class KernelCompiler:
    """Handles complete kernel compilation process including download and build"""

    @staticmethod
    def _progress_hook(block_num: int, block_size: int, total_size: int) -> None:
        """Progress hook for urlretrieve to display download progress"""
        if total_size <= 0:
            return

        downloaded = block_num * block_size
        percent = min(downloaded * 100 // total_size, 100)
        bar_length = 10
        filled = int(bar_length * downloaded // total_size)
        bar = "#" * filled + "-" * (bar_length - filled)

        if logger.getEffectiveLevel() <= logging.INFO:
            downloaded_mb = downloaded / (1024 * 1024)
            total_mb = total_size / (1024 * 1024)

            if sys.stdout.isatty():
                sys.stdout.write(
                    f"\rDownloading: |{bar}| {percent}% ({downloaded_mb:.2f}MB / {total_mb:.2f}MB)"
                )
                sys.stdout.flush()
                if downloaded >= total_size:
                    sys.stdout.write("\n")
                    sys.stdout.flush()
            else:
                if downloaded >= total_size or downloaded % (block_size * 100) == 0:
                    logger.info(
                        f"Downloading: {percent}% ({downloaded_mb:.2f}MB / {total_mb:.2f}MB)"
                    )

    def compile_from_version(self, spec: KernelSpec) -> KernelImage:
        """Complete compilation process from kernel version"""
        if spec.bzimage_path.exists():
            logger.info(f"Kernel {spec} already exists, skipping compilation")
            return KernelImage(spec.bzimage_path)

        try:
            self._download_source(spec)
            self._extract_source(spec)
            self._configure_kernel(spec)
            self._compile_kernel(spec)
            self._copy_bzimage(spec)

            logger.info(f"Successfully compiled kernel {spec}")
            return KernelImage(spec.bzimage_path)

        except Exception as e:
            logger.error(f"Failed to compile kernel {spec}: {e}")
            sys.exit(1)
        finally:
            # Always cleanup temporary files
            self._cleanup(spec)

    def _download_source(self, spec: KernelSpec) -> None:
        """Download kernel source tarball"""
        if spec.tarball_path.exists():
            logger.info(f"Tarball already exists: {spec.tarball_path}")
            return

        url_suffix = f"v{spec.major}.x/linux-{spec.version}.tar.xz"
        url = urljoin(config.config.kernel_tarball_url, url_suffix)

        logger.info(f"Downloading kernel from {url}")
        spec.tarball_path.parent.mkdir(parents=True, exist_ok=True)
        urlretrieve(url, spec.tarball_path, reporthook=self._progress_hook)
        logger.info("Kernel source downloaded")

    def _extract_source(self, spec: KernelSpec) -> None:
        """Extract kernel source tarball"""
        logger.info(f"Extracting kernel source to {spec.kernel_build_dir}")
        spec.kernel_build_dir.mkdir(parents=True, exist_ok=True)

        utils.run_command(
            [
                "tar",
                "-xf",
                str(spec.tarball_path),
                "-C",
                str(spec.kernel_build_dir),
                "--strip-components=1",
            ]
        )

    def _configure_kernel(self, spec: KernelSpec) -> None:
        """Configure kernel with provided config files"""
        config_path = spec.kernel_build_dir / ".config"

        with open(config_path, "wb") as kconfig:
            for config_rel_path in config.config.kconfig_rel_paths:
                config_abs_path = spec.kernel_build_dir / config_rel_path
                if config_abs_path.exists():
                    with open(config_abs_path, "rb") as conf:
                        kconfig.write(conf.read())

        logger.info("Updated kernel configuration")

    def _compile_kernel(self, spec: KernelSpec) -> None:
        """Compile the kernel"""
        logger.info(f"Compiling kernel in {spec.kernel_build_dir}")
        old_cwd = os.getcwd()

        try:
            os.chdir(spec.kernel_build_dir)
            # pahole is required for the DEBUG_INFO_BTF kernel configuration option.
            pahole_path = shutil.which("pahole")
            if pahole_path is None:
                logger.error(
                    "pahole not found in PATH. BTF generation requires pahole v1.16 or later."
                )
                sys.exit(1)
            friendly_cores = os.cpu_count() - 2
            utils.run_command(["make", "olddefconfig"], stream_output=True)
            logger.info("Starting kernel compilation")
            utils.run_command(
                ["make", f"-j{friendly_cores}", "bzImage"], stream_output=True
            )
            logger.info("Compiling bpftool")
            utils.run_command(
                ["make", "-C", "tools/bpf", f"-j{friendly_cores}", "bpftool"],
                stream_output=True,
            )
            logger.info("Compiling libbpf")
            utils.run_command(
                ["make", "-C", "tools/lib/bpf", f"-j{friendly_cores}"],
                stream_output=True,
            )
        except subprocess.CalledProcessError as e:
            logger.error(f"Kernel compilation failed: {e}")
            sys.exit(1)
        finally:
            os.chdir(old_cwd)

    def _copy_bzimage(self, spec: KernelSpec) -> None:
        """Copy compiled bzImage to final location"""
        # compile the bpftool as well
        src = spec.kernel_build_dir / "arch/x86/boot/bzImage"
        dest = spec.bzimage_path
        dest.parent.mkdir(parents=True, exist_ok=True)

        shutil.copy2(src, dest)
        logger.info(f"Stored bzImage at {dest}")

        shutil.copy2(
            spec.kernel_build_dir / "tools/bpf/bpftool/vmlinux.h", spec.vmlinux_path
        )
        logger.info(f"Stored vmlinux at {spec.vmlinux_path}")

        shutil.copy2(
            spec.kernel_build_dir / "tools/bpf/bpftool/bpftool", spec.bpftool_path
        )
        logger.info(f"Stored bpftool at {spec.bpftool_path}")

        shutil.copy2(spec.kernel_build_dir / "tools/lib/bpf/libbpf.a", spec.libbpf_path)
        logger.info(f"Stored libbpf at {spec.libbpf_path}")

    def _cleanup(self, spec: KernelSpec) -> None:
        """Clean up temporary files"""
        if spec.tarball_path.exists():
            spec.tarball_path.unlink()
            logger.info("Removed tarball")

        if spec.kernel_build_dir.exists():
            shutil.rmtree(spec.kernel_build_dir)
            logger.info("Removed kernel source directory")


class KernelManager:
    """Main interface for kernel management"""

    def __init__(self):
        self.compiler = KernelCompiler()

    def remove_kernel(self, name: str) -> None:
        """Remove compiled kernel by version"""
        version, _, arch = name.partition("-")
        spec = KernelSpec(version=version, arch=arch)
        if spec.kernel_dir.exists():
            shutil.rmtree(spec.kernel_dir)
            logger.info(f"Removed kernel {spec}")
        else:
            logger.error(
                f"Kernel {spec} does not exist, path {spec.kernel_dir} not found"
            )
            raise SystemExit(1)

    def build_kernel(self, version: str, arch=None) -> None:
        """Build kernel from version"""

        spec = KernelSpec(version=version, arch=arch)
        self.compiler.compile_from_version(spec)

    @staticmethod
    def get_kernel_from_version(
        version: str,
    ):
        """Get kernel image from version"""
        version, _, arch = version.partition("-")
        spec = KernelSpec(version=version, arch=arch)
        if spec.bzimage_path.exists():
            return spec, KernelImage(spec.bzimage_path)
        else:
            raise FileNotFoundError(
                f"Kernel {spec} not found. Use 'main.py kernel build' to create it."
            )

    def list_kernels(self) -> List[str]:
        """List all available compiled kernels"""
        if not config.config.kernels_dir.exists():
            raise FileNotFoundError(
                f"Kernels directory not found: {config.config.kernels_dir}"
            )

        kernels = []
        for file in config.config.kernels_dir.glob("linux-*"):
            if file.is_dir():
                match = re.match(r"linux-(.*)", file.name)
                if match:
                    kernels.append(match.group(1))

        return sorted(kernels)
