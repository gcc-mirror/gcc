import argparse
import logging
from pathlib import Path
import sys
import textwrap
import os

import bpf
import kernel
import vm
import config

logger = logging.getLogger(__name__)


def cmd_kernel_list(args):
    """List all available kernels"""
    kmanager = kernel.KernelManager()
    kernels = kmanager.list_kernels()
    if kernels:
        for k in kernels:
            print(k)
    else:
        logger.info("No kernels available")


def cmd_kernel_remove(args):
    """Remove a kernel"""
    kmanager = kernel.KernelManager()
    if not args.kernel:
        logger.error("kernel version required for remove action")
        sys.exit(1)
    kmanager.remove_kernel(args.kernel)
    logger.info(f"Kernel {args.kernel} removed")
    print(f"Kernel {args.kernel} removed")


def cmd_kernel_build(args):
    """Build a kernel"""
    kmanager = kernel.KernelManager()
    if not args.kernel:
        logger.error("kernel version required for build action")
        sys.exit(1)
    kmanager.build_kernel(version=args.kernel)


def cmd_bpf_compile(args):
    """Compile BPF source to bytecode only"""
    kmanager = kernel.KernelManager()

    try:
        kernel_spec, _ = kmanager.get_kernel_from_version(version=args.kernel)
    except Exception as e:
        logger.error(f"Failed to get kernel: {e}")
        sys.exit(1)

    try:
        bpf_program = bpf.BPFProgram(source_path=Path(args.bpf_src))
        output_path = bpf_program.compile_bpf(kernel_spec)

        if args.output:
            import shutil

            output_dest = Path(args.output)
            shutil.copy2(output_path, output_dest)
            logger.info(f"Copied to: {output_dest}")

    except Exception as e:
        logger.error(f"Failed to compile BPF source: {e}")
        sys.exit(1)


def cmd_vmtest(args):
    """Handle vmtest subcommand"""
    kmanager = kernel.KernelManager()

    try:
        kernel_spec, kernel_image = kmanager.get_kernel_from_version(
            version=args.kernel
        )
    except Exception as e:
        logger.error(f"Failed to get kernel: {e}")
        sys.exit(1)

    try:
        if args.bpf_src:
            command = bpf.BPFProgram.from_source(Path(args.bpf_src), kernel_spec)
        elif args.bpf_obj:
            command = bpf.BPFProgram.from_bpf_obj(Path(args.bpf_obj), kernel_spec)
        elif args.command:
            command = args.command
    except Exception as e:
        logger.error(f"Failed to prepare command for vmtest: {e}")
        sys.exit(1)

    virtual_machine = vm.VirtualMachine(kernel_image, args.rootfs, str(command))
    try:
        result = virtual_machine.execute()
    except vm.BootFailedError as e:
        logger.error(f"VM boot failure: {e}")
        sys.exit(e.returncode)

    if args.bpf_src or args.bpf_obj:
        if result.returncode == 0:
            print("BPF programs successfully loaded")
        else:
            if "Failed to load BPF skeleton" in result.stdout:
                print("BPF program failed to load")
                print("Verifier logs:")
                print(textwrap.indent(vm.bpf_verifier_logs(result.stdout), "\t"))
    elif args.command:
        print(result.stdout)

    sys.exit(result.returncode)


def main():
    parser = argparse.ArgumentParser(
        description="BPF vmtest tool",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent("""
            Examples:
              # Compile BPF source to bytecode
              %(prog)s bpf compile my_prog.bpf.c -o my_prog.bpf.o
              
              # Run BPF program in VM
              %(prog)s vmtest --kernel 6.15-x86_64 --bpf-src my_prog.bpf.c
              
              # List available kernels
              %(prog)s kernel list
        """),
    )

    parser.add_argument(
        "-v",
        "--log-level",
        help="Log level",
        metavar="DEBUG|INFO|WARNING|ERROR",
        choices=["DEBUG", "INFO", "WARNING", "ERROR"],
        default="INFO",
    )

    parser.add_argument(
        "--vmtest-dir",
        help="Directory for vmtest artifacts (or set VMTEST_DIR env variable)",
        metavar="DIR",
        type=str,
        default=os.getenv("VMTEST_DIR"),
    )

    subparsers = parser.add_subparsers(dest="subcommand", help="Available commands")

    # BPF subcommand
    bpf_subparser = subparsers.add_parser("bpf", help="BPF program management")
    bpf_subparsers = bpf_subparser.add_subparsers(dest="bpf_action", help="BPF actions")

    # bpf compile subcommand
    compile_parser = bpf_subparsers.add_parser(
        "compile", help="Compile BPF source to bytecode (.bpf.o)"
    )
    compile_parser.add_argument(
        "bpf_src",
        help="Path to BPF C source file",
        type=str,
    )
    compile_parser.add_argument(
        "-o",
        "--output",
        help="Output path for compiled bytecode (optional, defaults to temp dir)",
        metavar="PATH",
        type=str,
        required=True,
    )
    compile_parser.add_argument(
        "-k",
        "--kernel",
        help="Kernel version to use for compilation",
        metavar="VERSION",
        type=str,
        required=True,
    )

    compile_parser.set_defaults(func=cmd_bpf_compile)

    # VMtest subcommand
    vmtest_parser = subparsers.add_parser("vmtest", help="Run VM tests")
    vmtest_parser.set_defaults(func=cmd_vmtest)

    vmtest_parser.add_argument(
        "-k",
        "--kernel",
        help="Kernel version to boot in the vm",
        metavar="VERSION",
        type=str,
        required=True,
    )
    vmtest_parser.add_argument(
        "-r", "--rootfs", help="rootfs to mount in the vm", default="/", metavar="PATH"
    )
    command_group = vmtest_parser.add_mutually_exclusive_group(required=True)
    command_group.add_argument(
        "--bpf-src",
        help="Path to BPF C source file",
        metavar="PATH",
        type=str,
    )
    command_group.add_argument(
        "--bpf-obj",
        help="Path to bpf bytecode object",
        metavar="PATH",
        type=str,
    )
    command_group.add_argument(
        "-c", "--command", help="command to run in the vm", metavar="COMMAND"
    )

    # Kernel subcommand with nested subcommands
    kernel_subparser = subparsers.add_parser("kernel", help="Kernel management")
    kernel_subparsers = kernel_subparser.add_subparsers(
        dest="kernel_action", help="Kernel actions"
    )

    # kernel list subcommand
    list_parser = kernel_subparsers.add_parser(
        "list", help="List all available kernels"
    )
    list_parser.set_defaults(func=cmd_kernel_list)

    # kernel remove subcommand
    remove_parser = kernel_subparsers.add_parser("remove", help="Remove a kernel")
    remove_parser.add_argument(
        "kernel", help="Kernel version to remove (e.g., 6.15-x86_64)"
    )
    remove_parser.set_defaults(func=cmd_kernel_remove)

    # kernel build subcommand
    build_parser = kernel_subparsers.add_parser("build", help="Build a kernel")
    build_parser.add_argument(
        "kernel", help="Kernel version to build (e.g. 6.15-x86_64)"
    )
    build_parser.set_defaults(func=cmd_kernel_build)

    args = parser.parse_args()
    logging.basicConfig(level=args.log_level, format="%(levelname)s: %(message)s")

    if not args.vmtest_dir:
        logger.error(
            "VMTEST_DIR not specified. Use --vmtest-dir=DIR or set VMTEST_DIR environment variable"
        )
        sys.exit(1)

    vmtest_path = Path(args.vmtest_dir)
    if not vmtest_path.exists():
        logger.error(f"VMTEST_DIR does not exist: {vmtest_path}")
        sys.exit(1)

    if not vmtest_path.is_dir():
        logger.error(f"VMTEST_DIR is not a directory: {vmtest_path}")
        sys.exit(1)

    try:
        config.init_config(vmtest_dir=args.vmtest_dir)
    except ValueError as e:
        logger.error(str(e))
        sys.exit(1)

    logger.debug(f"VMTEST_DIR set to: {args.vmtest_dir}")

    if hasattr(args, "func"):
        args.func(args)
        sys.exit(0)
    else:
        parser.print_help()
        sys.exit(1)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        logger.error("Operation cancelled by user")
        sys.exit(1)
    except Exception as e:
        logger.error(f"Unknown error: {e}")
        sys.exit(1)
