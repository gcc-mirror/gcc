..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: M680x0

.. index:: M680x0 options

.. _m680x0-options:

M680x0 Options
^^^^^^^^^^^^^^

These are the :samp:`-m` options defined for M680x0 and ColdFire processors.
The default settings depend on which architecture was selected when
the compiler was configured; the defaults for the most common choices
are given below.

.. option:: -march={arch}

  Generate code for a specific M680x0 or ColdFire instruction set
  architecture.  Permissible values of :samp:`{arch}` for M680x0
  architectures are: :samp:`68000`, :samp:`68010`, :samp:`68020`,
  :samp:`68030`, :samp:`68040`, :samp:`68060` and :samp:`cpu32`.  ColdFire
  architectures are selected according to Freescale's ISA classification
  and the permissible values are: :samp:`isaa`, :samp:`isaaplus`,
  :samp:`isab` and :samp:`isac`.

  GCC defines a macro ``__mcfarch__`` whenever it is generating
  code for a ColdFire target.  The :samp:`{arch}` in this macro is one of the
  :option:`-march` arguments given above.

  When used together, :option:`-march` and :option:`-mtune` select code
  that runs on a family of similar processors but that is optimized
  for a particular microarchitecture.

.. option:: -mcpu={cpu}

  Generate code for a specific M680x0 or ColdFire processor.
  The M680x0 :samp:`{cpu}` s are: :samp:`68000`, :samp:`68010`, :samp:`68020`,
  :samp:`68030`, :samp:`68040`, :samp:`68060`, :samp:`68302`, :samp:`68332`
  and :samp:`cpu32`.  The ColdFire :samp:`{cpu}` s are given by the table
  below, which also classifies the CPUs into families:

  .. list-table::
     :header-rows: 1

     * - Family
       - :samp:`-mcpu` arguments

     * - :samp:`51`
       - :samp:`51` :samp:`51ac` :samp:`51ag` :samp:`51cn` :samp:`51em` :samp:`51je` :samp:`51jf` :samp:`51jg` :samp:`51jm` :samp:`51mm` :samp:`51qe` :samp:`51qm`
     * - :samp:`5206`
       - :samp:`5202` :samp:`5204` :samp:`5206`
     * - :samp:`5206e`
       - :samp:`5206e`
     * - :samp:`5208`
       - :samp:`5207` :samp:`5208`
     * - :samp:`5211a`
       - :samp:`5210a` :samp:`5211a`
     * - :samp:`5213`
       - :samp:`5211` :samp:`5212` :samp:`5213`
     * - :samp:`5216`
       - :samp:`5214` :samp:`5216`
     * - :samp:`52235`
       - :samp:`52230` :samp:`52231` :samp:`52232` :samp:`52233` :samp:`52234` :samp:`52235`
     * - :samp:`5225`
       - :samp:`5224` :samp:`5225`
     * - :samp:`52259`
       - :samp:`52252` :samp:`52254` :samp:`52255` :samp:`52256` :samp:`52258` :samp:`52259`
     * - :samp:`5235`
       - :samp:`5232` :samp:`5233` :samp:`5234` :samp:`5235` :samp:`523x`
     * - :samp:`5249`
       - :samp:`5249`
     * - :samp:`5250`
       - :samp:`5250`
     * - :samp:`5271`
       - :samp:`5270` :samp:`5271`
     * - :samp:`5272`
       - :samp:`5272`
     * - :samp:`5275`
       - :samp:`5274` :samp:`5275`
     * - :samp:`5282`
       - :samp:`5280` :samp:`5281` :samp:`5282` :samp:`528x`
     * - :samp:`53017`
       - :samp:`53011` :samp:`53012` :samp:`53013` :samp:`53014` :samp:`53015` :samp:`53016` :samp:`53017`
     * - :samp:`5307`
       - :samp:`5307`
     * - :samp:`5329`
       - :samp:`5327` :samp:`5328` :samp:`5329` :samp:`532x`
     * - :samp:`5373`
       - :samp:`5372` :samp:`5373` :samp:`537x`
     * - :samp:`5407`
       - :samp:`5407`
     * - :samp:`5475`
       - :samp:`5470` :samp:`5471` :samp:`5472` :samp:`5473` :samp:`5474` :samp:`5475` :samp:`547x` :samp:`5480` :samp:`5481` :samp:`5482` :samp:`5483` :samp:`5484` :samp:`5485`

  :option:`-mcpu=cpu` overrides :option:`-march=arch` if
  :samp:`{arch}` is compatible with :samp:`{cpu}`.  Other combinations of
  :option:`-mcpu` and :option:`-march` are rejected.

  GCC defines the macro ``__mcf_cpu_cpu`` when ColdFire target
  :samp:`{cpu}` is selected.  It also defines ``__mcf_family_family``,
  where the value of :samp:`{family}` is given by the table above.

.. option:: -mtune={tune}

  Tune the code for a particular microarchitecture within the
  constraints set by :option:`-march` and :option:`-mcpu`.
  The M680x0 microarchitectures are: :samp:`68000`, :samp:`68010`,
  :samp:`68020`, :samp:`68030`, :samp:`68040`, :samp:`68060`
  and :samp:`cpu32`.  The ColdFire microarchitectures
  are: :samp:`cfv1`, :samp:`cfv2`, :samp:`cfv3`, :samp:`cfv4` and :samp:`cfv4e`.

  You can also use :option:`-mtune=68020-40` for code that needs
  to run relatively well on 68020, 68030 and 68040 targets.
  :option:`-mtune=68020-60` is similar but includes 68060 targets
  as well.  These two options select the same tuning decisions as
  :option:`-m68020-40` and :option:`-m68020-60` respectively.

  GCC defines the macros ``__mcarch`` and ``__mcarch__``
  when tuning for 680x0 architecture :samp:`{arch}`.  It also defines
  ``mcarch`` unless either :option:`-ansi` or a non-GNU :option:`-std`
  option is used.  If GCC is tuning for a range of architectures,
  as selected by :option:`-mtune=68020-40` or :option:`-mtune=68020-60`,
  it defines the macros for every architecture in the range.

  GCC also defines the macro ``__muarch__`` when tuning for
  ColdFire microarchitecture :samp:`{uarch}`, where :samp:`{uarch}` is one
  of the arguments given above.

.. option:: -m68000, -mc68000

  Generate output for a 68000.  This is the default
  when the compiler is configured for 68000-based systems.
  It is equivalent to :option:`-march=68000`.

  Use this option for microcontrollers with a 68000 or EC000 core,
  including the 68008, 68302, 68306, 68307, 68322, 68328 and 68356.

.. option:: -m68010

  Generate output for a 68010.  This is the default
  when the compiler is configured for 68010-based systems.
  It is equivalent to :option:`-march=68010`.

.. option:: -m68020, -mc68020

  Generate output for a 68020.  This is the default
  when the compiler is configured for 68020-based systems.
  It is equivalent to :option:`-march=68020`.

.. option:: -m68030

  Generate output for a 68030.  This is the default when the compiler is
  configured for 68030-based systems.  It is equivalent to
  :option:`-march=68030`.

.. option:: -m68040

  Generate output for a 68040.  This is the default when the compiler is
  configured for 68040-based systems.  It is equivalent to
  :option:`-march=68040`.

  This option inhibits the use of 68881/68882 instructions that have to be
  emulated by software on the 68040.  Use this option if your 68040 does not
  have code to emulate those instructions.

.. option:: -m68060

  Generate output for a 68060.  This is the default when the compiler is
  configured for 68060-based systems.  It is equivalent to
  :option:`-march=68060`.

  This option inhibits the use of 68020 and 68881/68882 instructions that
  have to be emulated by software on the 68060.  Use this option if your 68060
  does not have code to emulate those instructions.

.. option:: -mcpu32

  Generate output for a CPU32.  This is the default
  when the compiler is configured for CPU32-based systems.
  It is equivalent to :option:`-march=cpu32`.

  Use this option for microcontrollers with a
  CPU32 or CPU32+ core, including the 68330, 68331, 68332, 68333, 68334,
  68336, 68340, 68341, 68349 and 68360.

.. option:: -m5200

  Generate output for a 520X ColdFire CPU.  This is the default
  when the compiler is configured for 520X-based systems.
  It is equivalent to :option:`-mcpu=5206`, and is now deprecated
  in favor of that option.

  Use this option for microcontroller with a 5200 core, including
  the MCF5202, MCF5203, MCF5204 and MCF5206.

.. option:: -m5206e

  Generate output for a 5206e ColdFire CPU.  The option is now
  deprecated in favor of the equivalent :option:`-mcpu=5206e`.

.. option:: -m528x

  Generate output for a member of the ColdFire 528X family.
  The option is now deprecated in favor of the equivalent
  :option:`-mcpu=528x`.

.. option:: -m5307

  Generate output for a ColdFire 5307 CPU.  The option is now deprecated
  in favor of the equivalent :option:`-mcpu=5307`.

.. option:: -m5407

  Generate output for a ColdFire 5407 CPU.  The option is now deprecated
  in favor of the equivalent :option:`-mcpu=5407`.

.. option:: -mcfv4e

  Generate output for a ColdFire V4e family CPU (e.g. 547x/548x).
  This includes use of hardware floating-point instructions.
  The option is equivalent to :option:`-mcpu=547x`, and is now
  deprecated in favor of that option.

.. option:: -m68020-40

  Generate output for a 68040, without using any of the new instructions.
  This results in code that can run relatively efficiently on either a
  68020/68881 or a 68030 or a 68040.  The generated code does use the
  68881 instructions that are emulated on the 68040.

  The option is equivalent to :option:`-march=68020` :option:`-mtune=68020-40`.

.. option:: -m68020-60

  Generate output for a 68060, without using any of the new instructions.
  This results in code that can run relatively efficiently on either a
  68020/68881 or a 68030 or a 68040.  The generated code does use the
  68881 instructions that are emulated on the 68060.

  The option is equivalent to :option:`-march=68020` :option:`-mtune=68020-60`.

.. option:: -mhard-float, -m68881

  Generate floating-point instructions.  This is the default for 68020
  and above, and for ColdFire devices that have an FPU.  It defines the
  macro ``__HAVE_68881__`` on M680x0 targets and ``__mcffpu__``
  on ColdFire targets.

.. option:: -msoft-float

  Do not generate floating-point instructions; use library calls instead.
  This is the default for 68000, 68010, and 68832 targets.  It is also
  the default for ColdFire devices that have no FPU.

.. option:: -mdiv, -mno-div

  Generate (do not generate) ColdFire hardware divide and remainder
  instructions.  If :option:`-march` is used without :option:`-mcpu`,
  the default is 'on' for ColdFire architectures and 'off' for M680x0
  architectures.  Otherwise, the default is taken from the target CPU
  (either the default CPU, or the one specified by :option:`-mcpu`).  For
  example, the default is 'off' for :option:`-mcpu=5206` and 'on' for
  :option:`-mcpu=5206e`.

  GCC defines the macro ``__mcfhwdiv__`` when this option is enabled.

.. option:: -mshort

  Consider type ``int`` to be 16 bits wide, like ``short int``.
  Additionally, parameters passed on the stack are also aligned to a
  16-bit boundary even on targets whose API mandates promotion to 32-bit.

.. option:: -mno-short

  Do not consider type ``int`` to be 16 bits wide.  This is the default.

.. option:: -mnobitfield, -mno-bitfield

  Do not use the bit-field instructions.  The :option:`-m68000`, :option:`-mcpu32`
  and :option:`-m5200` options imply :option:`-mnobitfield`.

.. option:: -mbitfield

  Do use the bit-field instructions.  The :option:`-m68020` option implies
  :option:`-mbitfield`.  This is the default if you use a configuration
  designed for a 68020.

.. option:: -mrtd

  Use a different function-calling convention, in which functions
  that take a fixed number of arguments return with the ``rtd``
  instruction, which pops their arguments while returning.  This
  saves one instruction in the caller since there is no need to pop
  the arguments there.

  This calling convention is incompatible with the one normally
  used on Unix, so you cannot use it if you need to call libraries
  compiled with the Unix compiler.

  Also, you must provide function prototypes for all functions that
  take variable numbers of arguments (including ``printf``);
  otherwise incorrect code is generated for calls to those
  functions.

  In addition, seriously incorrect code results if you call a
  function with too many arguments.  (Normally, extra arguments are
  harmlessly ignored.)

  The ``rtd`` instruction is supported by the 68010, 68020, 68030,
  68040, 68060 and CPU32 processors, but not by the 68000 or 5200.

  The default is :option:`-mno-rtd`.

.. option:: -malign-int, -mno-align-int

  Control whether GCC aligns ``int``, ``long``, ``long long``,
  ``float``, ``double``, and ``long double`` variables on a 32-bit
  boundary (:option:`-malign-int`) or a 16-bit boundary (:option:`-mno-align-int`).
  Aligning variables on 32-bit boundaries produces code that runs somewhat
  faster on processors with 32-bit busses at the expense of more memory.

  .. warning::

    If you use the :option:`-malign-int` switch, GCC
    aligns structures containing the above types differently than
    most published application binary interface specifications for the m68k.

  Use the pc-relative addressing mode of the 68000 directly, instead of
  using a global offset table.  At present, this option implies :option:`-fpic`,
  allowing at most a 16-bit offset for pc-relative addressing.  :option:`-fPIC` is
  not presently supported with :option:`-mpcrel`, though this could be supported for
  68020 and higher processors.

.. option:: -mno-strict-align, -mstrict-align

  Do not (do) assume that unaligned memory references are handled by
  the system.

.. option:: -msep-data

  Generate code that allows the data segment to be located in a different
  area of memory from the text segment.  This allows for execute-in-place in
  an environment without virtual memory management.  This option implies
  :option:`-fPIC`.

.. option:: -mno-sep-data

  Generate code that assumes that the data segment follows the text segment.
  This is the default.

.. option:: -mid-shared-library

  Generate code that supports shared libraries via the library ID method.
  This allows for execute-in-place and shared libraries in an environment
  without virtual memory management.  This option implies :option:`-fPIC`.

.. option:: -mno-id-shared-library

  Generate code that doesn't assume ID-based shared libraries are being used.
  This is the default.

.. option:: -mshared-library-id=n

  Specifies the identification number of the ID-based shared library being
  compiled.  Specifying a value of 0 generates more compact code; specifying
  other values forces the allocation of that number to the current
  library, but is no more space- or time-efficient than omitting this option.

.. option:: -mxgot, -mno-xgot

  When generating position-independent code for ColdFire, generate code
  that works if the GOT has more than 8192 entries.  This code is
  larger and slower than code generated without this option.  On M680x0
  processors, this option is not needed; :option:`-fPIC` suffices.

  GCC normally uses a single instruction to load values from the GOT.
  While this is relatively efficient, it only works if the GOT
  is smaller than about 64k.  Anything larger causes the linker
  to report an error such as:

  .. index:: relocation truncated to fit (ColdFire)

  .. code-block:: c++

    relocation truncated to fit: R_68K_GOT16O foobar

  If this happens, you should recompile your code with :option:`-mxgot`.
  It should then work with very large GOTs.  However, code generated with
  :option:`-mxgot` is less efficient, since it takes 4 instructions to fetch
  the value of a global symbol.

  Note that some linkers, including newer versions of the GNU linker,
  can create multiple GOTs and sort GOT entries.  If you have such a linker,
  you should only need to use :option:`-mxgot` when compiling a single
  object file that accesses more than 8192 GOT entries.  Very few do.

  These options have no effect unless GCC is generating
  position-independent code.

.. option:: -mlong-jump-table-offsets

  Use 32-bit offsets in ``switch`` tables.  The default is to use
  16-bit offsets.
