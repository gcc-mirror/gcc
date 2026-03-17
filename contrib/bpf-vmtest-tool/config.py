import platform
from pathlib import Path
import os
from dataclasses import dataclass


@dataclass
class VMTestConfig:
    """Configuration for BPF vmtest tool"""

    vmtest_dir: Path
    kernel_tarball_url: str = "https://cdn.kernel.org/pub/linux/kernel/"
    arch: str = platform.machine()
    vmtest_cc: str = os.getenv("VMTEST_CC", "gcc")
    vmtest_cflags: str = os.getenv("VMTEST_CFLAGS", "-g -Wall -Werror ")
    vmtest_ldflags: str = os.getenv("VMTEST_LDFLAGS", "-lelf -lz")
    bpf_cc: str = os.getenv("BPF_CC", "bpf-unknown-none-gcc")
    bpf_cflags: str = os.getenv("BPF_CFLAGS", "-O2 -Wall -Werror -c")
    bpf_includes: str = os.getenv("BPF_INCLUDES", "-I/usr/local/include -I/usr/include")

    @property
    def kconfig_rel_paths(self) -> list:
        """Kernel config paths relative to kernel directory"""
        return [
            "tools/testing/selftests/bpf/config",
            "tools/testing/selftests/bpf/config.vm",
            f"tools/testing/selftests/bpf/config.{self.arch}",
        ]

    @property
    def kernels_dir(self) -> Path:
        """Get kernels directory"""
        return self.vmtest_dir / "kernels"

    def __post_init__(self):
        """Validate vmtest_dir exists"""
        if not self.vmtest_dir.exists():
            raise ValueError(f"VMTEST_DIR does not exist: {self.vmtest_dir}")
        if not self.vmtest_dir.is_dir():
            raise ValueError(f"VMTEST_DIR is not a directory: {self.vmtest_dir}")


# Global config instance
config = None


def init_config(vmtest_dir: str):
    """Initialize global config"""
    global config
    config = VMTestConfig(vmtest_dir=Path(vmtest_dir))
