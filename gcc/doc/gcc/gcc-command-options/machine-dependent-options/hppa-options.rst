..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: HPPA

.. index:: HPPA Options

.. _hppa-options:

HPPA Options
^^^^^^^^^^^^

These :samp:`-m` options are defined for the HPPA family of computers:

.. option:: -march={architecture-type}

  Generate code for the specified architecture.  The choices for
  :samp:`{architecture-type}` are :samp:`1.0` for PA 1.0, :samp:`1.1` for PA
  1.1, and :samp:`2.0` for PA 2.0 processors.  Refer to
  :samp:`/usr/lib/sched.models` on an HP-UX system to determine the proper
  architecture option for your machine.  Code compiled for lower numbered
  architectures runs on higher numbered architectures, but not the
  other way around.

.. option:: -mpa-risc-1-0, -mpa-risc-1-1, -mpa-risc-2-0

  Synonyms for :option:`-march=1.0`, :option:`-march=1.1`, and :option:`-march=2.0` respectively.

.. option:: -mcaller-copies

  The caller copies function arguments passed by hidden reference.  This
  option should be used with care as it is not compatible with the default
  32-bit runtime.  However, only aggregates larger than eight bytes are
  passed by hidden reference and the option provides better compatibility
  with OpenMP.

.. option:: -mjump-in-delay

  This option is ignored and provided for compatibility purposes only.

.. option:: -mdisable-fpregs

  Prevent floating-point registers from being used in any manner.  This is
  necessary for compiling kernels that perform lazy context switching of
  floating-point registers.  If you use this option and attempt to perform
  floating-point operations, the compiler aborts.

.. option:: -mdisable-indexing

  Prevent the compiler from using indexing address modes.  This avoids some
  rather obscure problems when compiling MIG generated code under MACH.

.. option:: -mno-space-regs

  Generate code that assumes the target has no space registers.  This allows
  GCC to generate faster indirect calls and use unscaled index address modes.

  Such code is suitable for level 0 PA systems and kernels.

.. option:: -mspace-regs

  Default setting; overrides :option:`-mno-space-regs`.

.. option:: -mfast-indirect-calls

  Generate code that assumes calls never cross space boundaries.  This
  allows GCC to emit code that performs faster indirect calls.

  This option does not work in the presence of shared libraries or nested
  functions.

.. option:: -mfixed-range={register-range}

  Generate code treating the given register range as fixed registers.
  A fixed register is one that the register allocator cannot use.  This is
  useful when compiling kernel code.  A register range is specified as
  two registers separated by a dash.  Multiple register ranges can be
  specified separated by a comma.

.. option:: -mlong-load-store

  Generate 3-instruction load and store sequences as sometimes required by
  the HP-UX 10 linker.  This is equivalent to the :samp:`+k` option to
  the HP compilers.

.. option:: -mportable-runtime

  Use the portable calling conventions proposed by HP for ELF systems.

.. option:: -mgas

  Enable the use of assembler directives only GAS understands.

.. option:: -mschedule={cpu-type}

  Schedule code according to the constraints for the machine type
  :samp:`{cpu-type}`.  The choices for :samp:`{cpu-type}` are :samp:`700`
  :samp:`7100`, :samp:`7100LC`, :samp:`7200`, :samp:`7300` and :samp:`8000`.  Refer
  to :samp:`/usr/lib/sched.models` on an HP-UX system to determine the
  proper scheduling option for your machine.  The default scheduling is
  :samp:`8000`.

.. option:: -mlinker-opt

  Enable the optimization pass in the HP-UX linker.  Note this makes symbolic
  debugging impossible.  It also triggers a bug in the HP-UX 8 and HP-UX 9
  linkers in which they give bogus error messages when linking some programs.

.. option:: -msoft-float

  Generate output containing library calls for floating point.

  .. warning::

    The requisite libraries are not available for all HPPA
    targets.  Normally the facilities of the machine's usual C compiler are
    used, but this cannot be done directly in cross-compilation.  You must make
    your own arrangements to provide suitable library functions for
    cross-compilation.

  :option:`-msoft-float` changes the calling convention in the output file;
  therefore, it is only useful if you compile *all* of a program with
  this option.  In particular, you need to compile :samp:`libgcc.a`, the
  library that comes with GCC, with :option:`-msoft-float` in order for
  this to work.

.. option:: -msio

  Generate the predefine, ``_SIO``, for server IO.  The default is
  :option:`-mwsio`.  This generates the predefines, ``__hp9000s700``,
  ``__hp9000s700__`` and ``_WSIO``, for workstation IO.  These
  options are available under HP-UX and HI-UX.

.. option:: -mgnu-ld

  Use options specific to GNU :command:`ld`.
  This passes :option:`-shared` to :command:`ld` when
  building a shared library.  It is the default when GCC is configured,
  explicitly or implicitly, with the GNU linker.  This option does not
  affect which :command:`ld` is called; it only changes what parameters
  are passed to that :command:`ld`.
  The :command:`ld` that is called is determined by the
  :option:`--with-ld` configure option, GCC's program search path, and
  finally by the user's :envvar:`PATH`.  The linker used by GCC can be printed
  using :samp:`which `gcc -print-prog-name=ld``.  This option is only available
  on the 64-bit HP-UX GCC, i.e. configured with :samp:`hppa*64*-*-hpux*`.

.. option:: -mhp-ld

  Use options specific to HP :command:`ld`.
  This passes :option:`-b` to :command:`ld` when building
  a shared library and passes +Accept TypeMismatch to :command:`ld` on all
  links.  It is the default when GCC is configured, explicitly or
  implicitly, with the HP linker.  This option does not affect
  which :command:`ld` is called; it only changes what parameters are passed to that
  :command:`ld`.
  The :command:`ld` that is called is determined by the :option:`--with-ld`
  configure option, GCC's program search path, and finally by the user's
  :envvar:`PATH`.  The linker used by GCC can be printed using :samp:`which
  `gcc -print-prog-name=ld``.  This option is only available on the 64-bit
  HP-UX GCC, i.e. configured with :samp:`hppa*64*-*-hpux*`.

.. option:: -mlong-calls

  Generate code that uses long call sequences.  This ensures that a call
  is always able to reach linker generated stubs.  The default is to generate
  long calls only when the distance from the call site to the beginning
  of the function or translation unit, as the case may be, exceeds a
  predefined limit set by the branch type being used.  The limits for
  normal calls are 7,600,000 and 240,000 bytes, respectively for the
  PA 2.0 and PA 1.X architectures.  Sibcalls are always limited at
  240,000 bytes.

  Distances are measured from the beginning of functions when using the
  :option:`-ffunction-sections` option, or when using the :option:`-mgas`
  and :option:`-mno-portable-runtime` options together under HP-UX with
  the SOM linker.

  It is normally not desirable to use this option as it degrades
  performance.  However, it may be useful in large applications,
  particularly when partial linking is used to build the application.

  The types of long calls used depends on the capabilities of the
  assembler and linker, and the type of code being generated.  The
  impact on systems that support long absolute calls, and long pic
  symbol-difference or pc-relative calls should be relatively small.
  However, an indirect call is used on 32-bit ELF systems in pic code
  and it is quite long.

.. option:: -mno-long-calls

  Default setting; overrides :option:`-mlong-calls`.

.. option:: -munix={unix-std}

  Generate compiler predefines and select a startfile for the specified
  UNIX standard.  The choices for :samp:`{unix-std}` are :samp:`93`, :samp:`95`
  and :samp:`98`.  :samp:`93` is supported on all HP-UX versions.  :samp:`95`
  is available on HP-UX 10.10 and later.  :samp:`98` is available on HP-UX
  11.11 and later.  The default values are :samp:`93` for HP-UX 10.00,
  :samp:`95` for HP-UX 10.10 though to 11.00, and :samp:`98` for HP-UX 11.11
  and later.

  :option:`-munix=93` provides the same predefines as GCC 3.3 and 3.4.
  :option:`-munix=95` provides additional predefines for ``XOPEN_UNIX``
  and ``_XOPEN_SOURCE_EXTENDED``, and the startfile :samp:`unix95.o`.
  :option:`-munix=98` provides additional predefines for ``_XOPEN_UNIX``,
  ``_XOPEN_SOURCE_EXTENDED``, ``_INCLUDE__STDC_A1_SOURCE`` and
  ``_INCLUDE_XOPEN_SOURCE_500``, and the startfile :samp:`unix98.o`.

  It is *important* to note that this option changes the interfaces
  for various library routines.  It also affects the operational behavior
  of the C library.  Thus, *extreme* care is needed in using this
  option.

  Library code that is intended to operate with more than one UNIX
  standard must test, set and restore the variable ``__xpg4_extended_mask``
  as appropriate.  Most GNU software doesn't provide this capability.

.. option:: -nolibdld

  Suppress the generation of link options to search libdld.sl when the
  :option:`-static` option is specified on HP-UX 10 and later.

.. option:: -static

  The HP-UX implementation of setlocale in libc has a dependency on
  libdld.sl.  There isn't an archive version of libdld.sl.  Thus,
  when the :option:`-static` option is specified, special link options
  are needed to resolve this dependency.

  On HP-UX 10 and later, the GCC driver adds the necessary options to
  link with libdld.sl when the :option:`-static` option is specified.
  This causes the resulting binary to be dynamic.  On the 64-bit port,
  the linkers generate dynamic binaries by default in any case.  The
  :option:`-nolibdld` option can be used to prevent the GCC driver from
  adding these link options.

.. option:: -threads

  Add support for multithreading with the :dfn:`dce thread` library
  under HP-UX.  This option sets flags for both the preprocessor and
  linker.