..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: code generation, conventions, options, code generation, options, run-time

.. _code-gen-options:

Options for code generation conventions
***************************************

These machine-independent options control the interface conventions
used in code generation.

Most of them have both positive and negative forms; the negative form
of :samp:`-ffoo` would be :samp:`-fno-foo`.  In the table below, only
one of the forms is listed---the one which is not the default.  You
can figure out the other form by either removing no- or adding
it.

.. index:: fno-automatic, SAVE statement, statement, SAVE

.. option:: -fno-automatic

  Treat each program unit (except those marked as RECURSIVE) as if the
  ``SAVE`` statement were specified for every local variable and array
  referenced in it. Does not affect common blocks. (Some Fortran compilers
  provide this option under the name :option:`-static` or :option:`-save`.)
  The default, which is :option:`-fautomatic`, uses the stack for local
  variables smaller than the value given by :option:`-fmax-stack-var-size`.
  Use the option :option:`-frecursive` to use no static memory.

  Local variables or arrays having an explicit ``SAVE`` attribute are
  silently ignored unless the :option:`-pedantic` option is added.

.. index:: calling convention, f2c calling convention, g77 calling convention, libf2c calling convention

.. option:: -ff2c

  Generate code designed to be compatible with code generated
  by :command:`g77` and :command:`f2c`.

  The calling conventions used by :command:`g77` (originally implemented
  in :command:`f2c`) require functions that return type
  default ``REAL`` to actually return the C type ``double``, and
  functions that return type ``COMPLEX`` to return the values via an
  extra argument in the calling sequence that points to where to
  store the return value.  Under the default GNU calling conventions, such
  functions simply return their results as they would in GNU
  C---default ``REAL`` functions return the C type ``float``, and
  ``COMPLEX`` functions return the GNU C type ``complex``.
  Additionally, this option implies the :option:`-fsecond-underscore`
  option, unless :option:`-fno-second-underscore` is explicitly requested.

  This does not affect the generation of code that interfaces with
  the :command:`libgfortran` library.

  .. warning::

    It is not a good idea to mix Fortran code compiled with
    :option:`-ff2c` with code compiled with the default :option:`-fno-f2c`
    calling conventions as, calling ``COMPLEX`` or default ``REAL``
    functions between program parts which were compiled with different
    calling conventions will break at execution time.

  .. warning::

    This will break code which passes intrinsic functions
    of type default ``REAL`` or ``COMPLEX`` as actual arguments, as
    the library implementations use the :option:`-fno-f2c` calling conventions.

.. index:: fno-underscoring, underscore, symbol names, underscores, transforming symbol names, symbol names, transforming

.. option:: -fno-underscoring

  Do not transform names of entities specified in the Fortran
  source file by appending underscores to them.

  With :option:`-funderscoring` in effect, GNU Fortran appends one
  underscore to external names with no underscores.  This is done to ensure
  compatibility with code produced by many UNIX Fortran compilers.

  .. warning::

    The default behavior of GNU Fortran is
    incompatible with :command:`f2c` and :command:`g77`, please use the
    :option:`-ff2c` option if you want object files compiled with
    GNU Fortran to be compatible with object code created with these
    tools.

  Use of :option:`-fno-underscoring` is not recommended unless you are
  experimenting with issues such as integration of GNU Fortran into
  existing system environments (vis-ā-vis existing libraries, tools,
  and so on).

  For example, with :option:`-funderscoring`, and assuming that ``j()`` and
  ``max_count()`` are external functions while ``my_var`` and
  ``lvar`` are local variables, a statement like

  .. code-block:: fortran

    I = J() + MAX_COUNT (MY_VAR, LVAR)

  is implemented as something akin to:

  .. code-block:: fortran

    i = j_() + max_count__(&my_var__, &lvar);

  With :option:`-fno-underscoring`, the same statement is implemented as:

  .. code-block:: fortran

    i = j() + max_count(&my_var, &lvar);

  Use of :option:`-fno-underscoring` allows direct specification of
  user-defined names while debugging and when interfacing GNU Fortran
  code with other languages.

  Note that just because the names match does *not* mean that the
  interface implemented by GNU Fortran for an external name matches the
  interface implemented by some other language for that same name.
  That is, getting code produced by GNU Fortran to link to code produced
  by some other compiler using this or any other method can be only a
  small part of the overall solution---getting the code generated by
  both compilers to agree on issues other than naming can require
  significant effort, and, unlike naming disagreements, linkers normally
  cannot detect disagreements in these other areas.

  Also, note that with :option:`-fno-underscoring`, the lack of appended
  underscores introduces the very real possibility that a user-defined
  external name will conflict with a name in a system library, which
  could make finding unresolved-reference bugs quite difficult in some
  cases---they might occur at program run time, and show up only as
  buggy behavior at run time.

  In future versions of GNU Fortran we hope to improve naming and linking
  issues so that debugging always involves using the names as they appear
  in the source, even if the names as seen by the linker are mangled to
  prevent accidental linking between procedures with incompatible
  interfaces.

.. index:: fsecond-underscore, underscore, symbol names, underscores, transforming symbol names, symbol names, transforming, f2c calling convention, g77 calling convention, libf2c calling convention

.. option:: -fsecond-underscore

  By default, GNU Fortran appends an underscore to external
  names.  If this option is used GNU Fortran appends two
  underscores to names with underscores and one underscore to external names
  with no underscores.  GNU Fortran also appends two underscores to
  internal names with underscores to avoid naming collisions with external
  names.

  This option has no effect if :option:`-fno-underscoring` is
  in effect.  It is implied by the :option:`-ff2c` option.

  Otherwise, with this option, an external name such as ``MAX_COUNT``
  is implemented as a reference to the link-time external symbol
  ``max_count__``, instead of ``max_count_``.  This is required
  for compatibility with :command:`g77` and :command:`f2c`, and is implied
  by use of the :option:`-ff2c` option.

.. index:: fcoarray, coarrays

.. option:: -fcoarray={<keyword>}

  none
    Disable coarray support; using coarray declarations and image-control
    statements will produce a compile-time error. (Default)

  single
    Single-image mode, i.e. ``num_images()`` is always one.

  lib
    Library-based coarray parallelization; a suitable GNU Fortran coarray
    library needs to be linked.

.. index:: fcheck, array, bounds checking, bit intrinsics checking, bounds checking, pointer checking, memory checking, range checking, subscript checking, checking subscripts, run-time checking, checking array temporaries

.. option:: -fcheck={<keyword>}

  Enable the generation of run-time checks; the argument shall be
  a comma-delimited list of the following keywords.  Prefixing a check with
  no- disables it if it was activated by a previous specification.

  all
    Enable all run-time test of :option:`-fcheck`.

  array-temps
    Warns at run time when for passing an actual argument a temporary array
    had to be generated. The information generated by this warning is
    sometimes useful in optimization, in order to avoid such temporaries.

    Note: The warning is only printed once per location.

  bits
    Enable generation of run-time checks for invalid arguments to the bit
    manipulation intrinsics.

  bounds
    Enable generation of run-time checks for array subscripts
    and against the declared minimum and maximum values.  It also
    checks array indices for assumed and deferred
    shape arrays against the actual allocated bounds and ensures that all string
    lengths are equal for character array constructors without an explicit
    typespec.

    Some checks require that :option:`-fcheck=bounds` is set for
    the compilation of the main program.

    Note: In the future this may also include other forms of checking, e.g.,
    checking substring references.

  do
    Enable generation of run-time checks for invalid modification of loop
    iteration variables.

  mem
    Enable generation of run-time checks for memory allocation.
    Note: This option does not affect explicit allocations using the
    ``ALLOCATE`` statement, which will be always checked.

  pointer
    Enable generation of run-time checks for pointers and allocatables.

  recursion
    Enable generation of run-time checks for recursively called subroutines and
    functions which are not marked as recursive. See also :option:`-frecursive`.
    Note: This check does not work for OpenMP programs and is disabled if used
    together with :option:`-frecursive` and :option:`-fopenmp`.

    Example: Assuming you have a file :samp:`foo.f90`, the command

  .. code-block:: bash

      gfortran -fcheck=all,no-array-temps foo.f90

  will compile the file with all checks enabled as specified above except
  warnings for generated array temporaries.

.. index:: fbounds-check

.. option:: -fbounds-check

  .. Note: This option is also referred in gcc's manpage

  Deprecated alias for :option:`-fcheck=bounds`.

.. index:: tail-call-workaround

.. option:: -ftail-call-workaround, -ftail-call-workaround={n}

  Some C interfaces to Fortran codes violate the gfortran ABI by
  omitting the hidden character length arguments as described in
  See :ref:`argument-passing-conventions`.  This can lead to crashes
  because pushing arguments for tail calls can overflow the stack.

  To provide a workaround for existing binary packages, this option
  disables tail call optimization for gfortran procedures with character
  arguments.  With :option:`-ftail-call-workaround=2` tail call optimization
  is disabled in all gfortran procedures with character arguments,
  with :option:`-ftail-call-workaround=1` or equivalent
  :option:`-ftail-call-workaround` only in gfortran procedures with character
  arguments that call implicitly prototyped procedures.

  Using this option can lead to problems including crashes due to
  insufficient stack space.

  It is *very strongly* recommended to fix the code in question.
  The :option:`-fc-prototypes-external` option can be used to generate
  prototypes which conform to gfortran's ABI, for inclusion in the
  source code.

  Support for this option will likely be withdrawn in a future release
  of gfortran.

  The negative form, :option:`-fno-tail-call-workaround` or equivalent
  :option:`-ftail-call-workaround=0`, can be used to disable this option.

  Default is currently :option:`-ftail-call-workaround`, this will change
  in future releases.

.. index:: fcheck-array-temporaries

.. option:: -fcheck-array-temporaries

  Deprecated alias for :option:`-fcheck=array-temps`.

.. index:: fmax-array-constructor

.. option:: -fmax-array-constructor={n}

  This option can be used to increase the upper limit permitted in
  array constructors.  The code below requires this option to expand
  the array at compile time.

  .. code-block:: fortran

    program test
    implicit none
    integer j
    integer, parameter :: n = 100000
    integer, parameter :: i(n) = (/ (2*j, j = 1, n) /)
    print '(10(I0,1X))', i
    end program test

  .. warning::
    This option can lead to long compile times and excessively
    large object files.

  The default value for :samp:`{n}` is 65535.

.. index:: fmax-stack-var-size

.. option:: -fmax-stack-var-size={n}

  This option specifies the size in bytes of the largest array that will be put
  on the stack; if the size is exceeded static memory is used (except in
  procedures marked as RECURSIVE). Use the option :option:`-frecursive` to
  allow for recursive procedures which do not have a RECURSIVE attribute or
  for parallel programs. Use :option:`-fno-automatic` to never use the stack.

  This option currently only affects local arrays declared with constant
  bounds, and may not apply to all character variables.
  Future versions of GNU Fortran may improve this behavior.

  The default value for :samp:`{n}` is 65536.

.. index:: fstack-arrays

.. option:: -fstack-arrays

  Adding this option will make the Fortran compiler put all arrays of
  unknown size and array temporaries onto stack memory.  If your program uses very
  large local arrays it is possible that you will have to extend your runtime
  limits for stack memory on some operating systems. This flag is enabled
  by default at optimization level :option:`-Ofast` unless
  :option:`-fmax-stack-var-size` is specified.

.. index:: fpack-derived, structure packing

.. option:: -fpack-derived

  This option tells GNU Fortran to pack derived type members as closely as
  possible.  Code compiled with this option is likely to be incompatible
  with code compiled without this option, and may execute slower.

.. index:: frepack-arrays, repacking arrays

.. option:: -frepack-arrays

  In some circumstances GNU Fortran may pass assumed shape array
  sections via a descriptor describing a noncontiguous area of memory.
  This option adds code to the function prologue to repack the data into
  a contiguous block at runtime.

  This should result in faster accesses to the array.  However it can introduce
  significant overhead to the function call, especially  when the passed data
  is noncontiguous.

.. index:: fshort-enums

.. option:: -fshort-enums

  This option is provided for interoperability with C code that was
  compiled with the :option:`-fshort-enums` option.  It will make
  GNU Fortran choose the smallest ``INTEGER`` kind a given
  enumerator set will fit in, and give all its enumerators this kind.

.. index:: finline-arg-packing

.. option:: -finline-arg-packing

  When passing an assumed-shape argument of a procedure as actual
  argument to an assumed-size or explicit size or as argument to a
  procedure that does not have an explicit interface, the argument may
  have to be packed, that is put into contiguous memory. An example is
  the call to ``foo`` in

  .. code-block:: fortran

      subroutine foo(a)
         real, dimension(*) :: a
      end subroutine foo
      subroutine bar(b)
         real, dimension(:) :: b
         call foo(b)
      end subroutine bar

  When :option:`-finline-arg-packing` is in effect, this packing will be
  performed by inline code. This allows for more optimization while
  increasing code size.

  :option:`-finline-arg-packing` is implied by any of the :option:`-O` options
  except when optimizing for size via :option:`-Os`.  If the code
  contains a very large number of argument that have to be packed, code
  size and also compilation time may become excessive.  If that is the
  case, it may be better to disable this option.  Instances of packing
  can be found by using :option:`-Warray-temporaries`.

.. index:: fexternal-blas

.. option:: -fexternal-blas

  This option will make :command:`gfortran` generate calls to BLAS functions
  for some matrix operations like ``MATMUL``, instead of using our own
  algorithms, if the size of the matrices involved is larger than a given
  limit (see :option:`-fblas-matmul-limit`).  This may be profitable if an
  optimized vendor BLAS library is available.  The BLAS library will have
  to be specified at link time.

.. index:: fblas-matmul-limit

.. option:: -fblas-matmul-limit={n}

  Only significant when :option:`-fexternal-blas` is in effect.
  Matrix multiplication of matrices with size larger than (or equal to) :samp:`{n}`
  will be performed by calls to BLAS functions, while others will be
  handled by :command:`gfortran` internal algorithms. If the matrices
  involved are not square, the size comparison is performed using the
  geometric mean of the dimensions of the argument and result matrices.

  The default value for :samp:`{n}` is 30.

.. index:: finline-matmul-limit

.. option:: -finline-matmul-limit={n}

  When front-end optimization is active, some calls to the ``MATMUL``
  intrinsic function will be inlined.  This may result in code size
  increase if the size of the matrix cannot be determined at compile
  time, as code for both cases is generated.  Setting
  ``-finline-matmul-limit=0`` will disable inlining in all cases.
  Setting this option with a value of :samp:`{n}` will produce inline code
  for matrices with size up to :samp:`{n}`. If the matrices involved are not
  square, the size comparison is performed using the geometric mean of
  the dimensions of the argument and result matrices.

  The default value for :samp:`{n}` is 30.  The ``-fblas-matmul-limit``
  can be used to change this value.

.. index:: frecursive

.. option:: -frecursive

  Allow indirect recursion by forcing all local arrays to be allocated
  on the stack. This flag cannot be used together with
  :option:`-fmax-stack-var-size=` or :option:`-fno-automatic`.

.. index:: finit-local-zero, finit-derived, finit-integer, finit-real, finit-logical, finit-character

.. option:: -finit-local-zero
            -finit-derived
            -finit-integer={n}
            -finit-real={<zero|inf|-inf|nan|snan>}
            -finit-logical={<true|false>}
            -finit-character={n}

  The :option:`-finit-local-zero` option instructs the compiler to
  initialize local ``INTEGER``, ``REAL``, and ``COMPLEX``
  variables to zero, ``LOGICAL`` variables to false, and
  ``CHARACTER`` variables to a string of null bytes.  Finer-grained
  initialization options are provided by the
  :option:`-finit-integer=n`,
  :option:`-finit-real=<zero|inf|-inf|nan|snan>` (which also initializes
  the real and imaginary parts of local ``COMPLEX`` variables),
  :option:`-finit-logical=<true|false>`, and
  :option:`-finit-character=n` (where :samp:`{n}` is an ASCII character
  value) options.

  With :option:`-finit-derived`, components of derived type variables will be
  initialized according to these flags.  Components whose type is not covered by
  an explicit :option:`-finit-*` flag will be treated as described above with
  :option:`-finit-local-zero`.

  These options do not initialize

  * objects with the POINTER attribute

  * allocatable arrays

  * variables that appear in an ``EQUIVALENCE`` statement.

  (These limitations may be removed in future releases).

  Note that the :option:`-finit-real=nan` option initializes ``REAL``
  and ``COMPLEX`` variables with a quiet NaN. For a signalling NaN
  use :option:`-finit-real=snan` ; note, however, that compile-time
  optimizations may convert them into quiet NaN and that trapping
  needs to be enabled (e.g. via :option:`-ffpe-trap`).

  The :option:`-finit-integer` option will parse the value into an
  integer of type ``INTEGER(kind=C_LONG)`` on the host.  Said value
  is then assigned to the integer variables in the Fortran code, which
  might result in wraparound if the value is too large for the kind.

  Finally, note that enabling any of the :option:`-finit-*` options will
  silence warnings that would have been emitted by :option:`-Wuninitialized`
  for the affected local variables.

.. index:: falign-commons, alignment of COMMON blocks

.. option:: -falign-commons

  By default, :command:`gfortran` enforces proper alignment of all variables in a
  ``COMMON`` block by padding them as needed. On certain platforms this is mandatory,
  on others it increases performance. If a ``COMMON`` block is not declared with
  consistent data types everywhere, this padding can cause trouble, and
  :option:`-fno-align-commons` can be used to disable automatic alignment. The
  same form of this option should be used for all files that share a ``COMMON`` block.
  To avoid potential alignment issues in ``COMMON`` blocks, it is recommended to order
  objects from largest to smallest.

.. index:: fno-protect-parens, re-association of parenthesized expressions

.. option:: -fno-protect-parens

  By default the parentheses in expression are honored for all optimization
  levels such that the compiler does not do any re-association. Using
  :option:`-fno-protect-parens` allows the compiler to reorder ``REAL`` and
  ``COMPLEX`` expressions to produce faster code. Note that for the re-association
  optimization :option:`-fno-signed-zeros` and :option:`-fno-trapping-math`
  need to be in effect. The parentheses protection is enabled by default, unless
  :option:`-Ofast` is given.

.. index:: frealloc-lhs, Reallocate the LHS in assignments

.. option:: -frealloc-lhs

  An allocatable left-hand side of an intrinsic assignment is automatically
  (re)allocated if it is either unallocated or has a different shape. The
  option is enabled by default except when :option:`-std=f95` is given. See
  also :option:`-Wrealloc-lhs`.

.. index:: faggressive-function-elimination, Elimination of functions with identical argument lists

.. option:: -faggressive-function-elimination

  Functions with identical argument lists are eliminated within
  statements, regardless of whether these functions are marked
  ``PURE`` or not. For example, in

  .. code-block:: fortran

      a = f(b,c) + f(b,c)

  there will only be a single call to ``f``.  This option only works
  if :option:`-ffrontend-optimize` is in effect.

.. index:: frontend-optimize, Front-end optimization

.. option:: -ffrontend-optimize

  This option performs front-end optimization, based on manipulating
  parts the Fortran parse tree.  Enabled by default by any :option:`-O` option
  except :option:`-O0` and :option:`-Og`.  Optimizations enabled by this option
  include:

  * inlining calls to ``MATMUL``,

  * elimination of identical function calls within expressions,

  * removing unnecessary calls to ``TRIM`` in comparisons and assignments,

  * replacing ``TRIM(a)`` with ``a(1:LEN_TRIM(a))`` and

  * short-circuiting of logical operators (``.AND.`` and ``.OR.``).

  It can be deselected by specifying :option:`-fno-frontend-optimize`.

.. index:: frontend-loop-interchange, loop interchange, Fortran

.. option:: -ffrontend-loop-interchange

  Attempt to interchange loops in the Fortran front end where
  profitable.  Enabled by default by any :option:`-O` option.
  At the moment, this option only affects ``FORALL`` and
  ``DO CONCURRENT`` statements with several forall triplets.

See :ref:`gcc:code-gen-options`, for information on more options
offered by the GBE
shared by :command:`gfortran`, :command:`gcc`, and other GNU compilers.
