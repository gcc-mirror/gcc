import logging
from pathlib import Path
import subprocess
from typing import List

from kernel import KernelImage

logger = logging.getLogger(__name__)


class VMConfig:
    """Configuration container for VM settings"""

    def __init__(
        self, kernel_image: KernelImage, rootfs_path: str, command: str, **kwargs
    ):
        self.kernel = kernel_image
        self.kernel_path = str(kernel_image.path)
        self.rootfs_path = rootfs_path
        self.command = command
        self.memory_mb = kwargs.get("memory_mb", 512)
        self.cpu_count = kwargs.get("cpu_count", 1)
        self.extra_args = kwargs.get("extra_args", {})


def bpf_verifier_logs(output: str) -> str:
    start_tag = "--- Verifier log start ---"
    end_tag = "--- Verifier log end ---"

    start_idx = output.find(start_tag)
    end_idx = output.find(end_tag)

    if start_idx != -1 and end_idx != -1:
        # Extract between the tags (excluding the markers themselves)
        log_body = output[start_idx + len(start_tag) : end_idx].strip()
        return log_body
    else:
        return "No verifier log found in the output."


class Vmtest:
    """vmtest backend implementation"""

    def __init__(self):
        pass

    def _boot_command(self, vm_config: VMConfig):
        vmtest_command = ["vmtest"]
        vmtest_command.extend(["-k", vm_config.kernel_path])
        vmtest_command.extend(["-r", vm_config.rootfs_path])
        # If it is a compiled BPF program, use the mounted path inside the VM
        if vm_config.command.endswith(".bin"):
            vmtest_command.append("/mnt/vmtest/" + Path(vm_config.command).name)
        else: 
            vmtest_command.append(vm_config.command)
        return vmtest_command

    def _remove_boot_log(self, full_output: str) -> str:
        """
        Filters QEMU and kernel boot logs, returning only the output after the
        `===> Running command` marker.
        """
        marker = "===> Running command"
        lines = full_output.splitlines()

        try:
            start_index = next(i for i, line in enumerate(lines) if marker in line)
            # Return everything after that marker (excluding the marker itself)
            return "\n".join(lines[start_index + 1 :]).strip()
        except StopIteration:
            return full_output.strip()

    def run_command(self, vm_config):
        vm = None
        try:
            logger.info(f"Booting VM with kernel: {vm_config.kernel_path}")
            logger.info(f"Using rootfs: {vm_config.rootfs_path}")
            vm = subprocess.run(
                self._boot_command(vm_config),
                check=True,
                text=True,
                capture_output=True,
                shell=False,
                cwd=Path(vm_config.command).parent,
            )
            vm_stdout = vm.stdout
            logger.debug(vm_stdout)
            return VMCommandResult(
                vm.returncode, self._remove_boot_log(vm_stdout), None
            )
        except FileNotFoundError:
            raise BootFailedError(
                "vmtest command not found in PATH. Please ensure vmtest is installed and available in your system PATH."
            )
        except subprocess.CalledProcessError as e:
            out = e.stdout
            err = e.stderr
            # when the command in the vm fails we consider it as a successful boot
            if "===> Running command" not in out:
                raise BootFailedError("Boot failed", out, err, e.returncode)
            logger.debug("STDOUT: \n%s", out)
            logger.debug("STDERR: \n%s", err)
            return VMCommandResult(e.returncode, self._remove_boot_log(out), err)


class VMCommandResult:
    def __init__(self, returncode, stdout, stderr) -> None:
        self.returncode = returncode
        self.stdout = stdout
        self.stderr = stderr


class VirtualMachine:
    """Main VM class - simple interface for end users"""

    # Registry of available hypervisors
    _hypervisors = {
        "vmtest": Vmtest,
    }

    def __init__(
        self,
        kernel_image: KernelImage,
        rootfs_path: str,
        command: str,
        hypervisor_type: str = "vmtest",
        **kwargs,
    ):
        self.config = VMConfig(kernel_image, rootfs_path, command, **kwargs)

        if hypervisor_type not in self._hypervisors:
            raise ValueError(f"Unsupported hypervisor: {hypervisor_type}")

        self.hypervisor = self._hypervisors[hypervisor_type]()

    @classmethod
    def list_hypervisors(cls) -> List[str]:
        """List available hypervisors"""
        return list(cls._hypervisors.keys())

    def execute(self):
        """Execute command in VM"""
        return self.hypervisor.run_command(self.config)


class BootFailedError(Exception):
    """Raised when VM fails to boot properly (before command execution)."""

    def __init__(
        self, message: str, stdout: str = "", stderr: str = "", returncode: int = -1
    ):
        super().__init__(message)
        self.stdout = stdout
        self.stderr = stderr
        self.returncode = returncode

    def __str__(self):
        base = super().__str__()

        output_parts = [
            base,
            f"Return code: {self.returncode}",
        ]

        optional_sections = [
            ("STDOUT", self.stdout),
            ("STDERR", self.stderr),
        ]

        for header, content in optional_sections:
            if content:
                output_parts.append(f"--- {header} ---")
                output_parts.append(content)

        return "\n".join(output_parts)
