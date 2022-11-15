..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _top-level:

Top Level Source Directory
**************************

The top level source directory in a GCC distribution contains several
files and directories that are shared with other software
distributions such as that of GNU Binutils.  It also contains several
subdirectories that contain parts of GCC and its runtime libraries:

:samp:`c++tools`
  Contains the sources for the g++-mapper-server, a tool used with
  C++ modules.

:samp:`config`
  Autoconf macros and Makefile fragments used throughout the tree.

:samp:`contrib`
  Contributed scripts that may be found useful in conjunction with GCC.

:samp:`fixincludes`
  The support for fixing system headers to work with GCC.  See
  :samp:`fixincludes/README` for more information.  The headers fixed by
  this mechanism are installed in :samp:`{libsubdir}/include-fixed`.
  Along with those headers, :samp:`README-fixinc` is also installed, as
  :samp:`{libsubdir}/include-fixed/README`.

:samp:`gcc`
  The main sources of GCC itself (except for runtime libraries),
  including optimizers, support for different target architectures,
  language front ends, and testsuites.  See :ref:`gcc-directory`, for details.

:samp:`gnattools`
  Support tools for GNAT.

:samp:`gotools`
  Support tools for Go.

:samp:`include`
  Headers for the ``libiberty`` library.

:samp:`intl`
  GNU ``libintl``, from GNU ``gettext``, for systems which do not
  include it in ``libc``.

:samp:`libada`
  The Ada runtime library.

:samp:`libatomic`
  The runtime support library for atomic operations (e.g. for ``__sync``
  and ``__atomic``).

:samp:`libbacktrace`
  A library that allows gcc to produce backtraces when it crashes.

:samp:`libcc1`
  A library that allows gdb to make use of the compiler.

:samp:`libcody`
  A compiler dynamism library to allow communication between compilers and
  build systems, for purposes such as C++ modules.

:samp:`libcpp`
  The C preprocessor library.

:samp:`libdecnumber`
  The Decimal Float support library.

:samp:`libffi`
  The ``libffi`` library, used as part of the Go runtime library.

:samp:`libgcc`
  The GCC runtime library.

:samp:`libgfortran`
  The Fortran runtime library.

:samp:`libgo`
  The Go runtime library.  The bulk of this library is mirrored from the
  `master Go repository <https://github.com/golang/go>`_.

:samp:`libgomp`
  The GNU Offloading and Multi Processing Runtime Library.

:samp:`libiberty`
  The ``libiberty`` library, used for portability and for some
  generally useful data structures and algorithms.  See :ref:`libiberty:top`, for more information
  about this library.

:samp:`libitm`
  The runtime support library for transactional memory.

:samp:`libobjc`
  The Objective-C and Objective-C++ runtime library.

:samp:`libphobos`
  The D standard and runtime library.  The bulk of this library is mirrored
  from the `master D repositories <https://github.com/dlang>`_.

:samp:`libquadmath`
  The runtime support library for quad-precision math operations.

:samp:`libsanitizer`
  Libraries for various sanitizers.  The bulk of this directory is mirrored
  from the `Google sanitizers
  repositories <https://github.com/google/sanitizers>`_.

:samp:`libssp`
  The Stack protector runtime library.

:samp:`libstdc++-v3`
  The C++ runtime library.

:samp:`libvtv`
  The vtable verification library.

:samp:`lto-plugin`
  Plugin used by the linker if link-time optimizations are enabled.

:samp:`maintainer-scripts`
  Scripts used by the ``gccadmin`` account on ``gcc.gnu.org``.

:samp:`zlib`
  The ``zlib`` compression library, used for compressing and
  uncompressing GCC's intermediate language in LTO object files.

The build system in the top level directory, including how recursion
into subdirectories works and how building runtime libraries for
multilibs is handled, is documented in a separate manual, included
with GNU Binutils.
