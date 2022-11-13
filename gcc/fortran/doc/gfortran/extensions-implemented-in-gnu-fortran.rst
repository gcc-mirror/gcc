..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: extensions, implemented

.. _extensions-implemented-in-gnu-fortran:

Extensions implemented in GNU Fortran
*************************************

GNU Fortran implements a number of extensions over standard Fortran.
This chapter contains information on their syntax and meaning.  There
are currently two categories of GNU Fortran extensions, those that
provide functionality beyond that provided by any standard, and those
that are supported by GNU Fortran purely for backward compatibility
with legacy compilers.  By default, :option:`-std=gnu` allows the
compiler to accept both types of extensions, but to warn about the use
of the latter.  Specifying either :option:`-std=f95`,
:option:`-std=f2003`, :option:`-std=f2008`, or :option:`-std=f2018`
disables both types of extensions, and :option:`-std=legacy` allows
both without warning.  The special compile flag :option:`-fdec` enables
additional compatibility extensions along with those enabled by
:option:`-std=legacy`.

.. toctree::
  :maxdepth: 2


.. index:: kind, old-style

.. _old-style-kind-specifications:

Old-style kind specifications
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GNU Fortran allows old-style kind specifications in declarations.  These
look like:

.. code-block:: fortran

        TYPESPEC*size x,y,z

where ``TYPESPEC`` is a basic type (``INTEGER``, ``REAL``,
etc.), and where ``size`` is a byte count corresponding to the
storage size of a valid kind for that type.  (For ``COMPLEX``
variables, ``size`` is the total size of the real and imaginary
parts.)  The statement then declares ``x``, ``y`` and ``z`` to
be of type ``TYPESPEC`` with the appropriate kind.  This is
equivalent to the standard-conforming declaration

.. code-block:: fortran

        TYPESPEC(k) x,y,z

where ``k`` is the kind parameter suitable for the intended precision.  As
kind parameters are implementation-dependent, use the ``KIND``,
``SELECTED_INT_KIND`` and ``SELECTED_REAL_KIND`` intrinsics to retrieve
the correct value, for instance ``REAL*8 x`` can be replaced by:

.. code-block:: fortran

  INTEGER, PARAMETER :: dbl = KIND(1.0d0)
  REAL(KIND=dbl) :: x

.. _old-style-variable-initialization:

Old-style variable initialization
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GNU Fortran allows old-style initialization of variables of the
form:

.. code-block:: fortran

        INTEGER i/1/,j/2/
        REAL x(2,2) /3*0.,1./

The syntax for the initializers is as for the ``DATA`` statement, but
unlike in a ``DATA`` statement, an initializer only applies to the
variable immediately preceding the initialization.  In other words,
something like ``INTEGER I,J/2,3/`` is not valid.  This style of
initialization is only allowed in declarations without double colons
(``::``); the double colons were introduced in Fortran 90, which also
introduced a standard syntax for initializing variables in type
declarations.

Examples of standard-conforming code equivalent to the above example
are:

.. code-block:: fortran

  ! Fortran 90
        INTEGER :: i = 1, j = 2
        REAL :: x(2,2) = RESHAPE((/0.,0.,0.,1./),SHAPE(x))
  ! Fortran 77
        INTEGER i, j
        REAL x(2,2)
        DATA i/1/, j/2/, x/3*0.,1./

Note that variables which are explicitly initialized in declarations
or in ``DATA`` statements automatically acquire the ``SAVE``
attribute.

.. index:: Namelist

.. _extensions-to-namelist:

Extensions to namelist
^^^^^^^^^^^^^^^^^^^^^^

GNU Fortran fully supports the Fortran 95 standard for namelist I/O
including array qualifiers, substrings and fully qualified derived types.
The output from a namelist write is compatible with namelist read.  The
output has all names in upper case and indentation to column 1 after the
namelist name.  Two extensions are permitted:

Old-style use of :samp:`$` instead of :samp:`&`

.. code-block::

  $MYNML
   X(:)%Y(2) = 1.0 2.0 3.0
   CH(1:4) = "abcd"
  $END

It should be noted that the default terminator is :samp:`/` rather than
:samp:`&END`.

Querying of the namelist when inputting from stdin.  After at least
one space, entering :samp:`?` sends to stdout the namelist name and the names of
the variables in the namelist:

.. code-block::

   ?

  &mynml
   x
   x%y
   ch
  &end

Entering :samp:`=?` outputs the namelist to stdout, as if
``WRITE(*,NML = mynml)`` had been called:

.. code-block::

  =?

  &MYNML
   X(1)%Y=  0.000000    ,  1.000000    ,  0.000000    ,
   X(2)%Y=  0.000000    ,  2.000000    ,  0.000000    ,
   X(3)%Y=  0.000000    ,  3.000000    ,  0.000000    ,
   CH=abcd,  /

To aid this dialog, when input is from stdin, errors send their
messages to stderr and execution continues, even if ``IOSTAT`` is set.

``PRINT`` namelist is permitted.  This causes an error if
:option:`-std=f95` is used.

.. code-block:: fortran

  PROGRAM test_print
    REAL, dimension (4)  ::  x = (/1.0, 2.0, 3.0, 4.0/)
    NAMELIST /mynml/ x
    PRINT mynml
  END PROGRAM test_print

Expanded namelist reads are permitted.  This causes an error if
:option:`-std=f95` is used.  In the following example, the first element
of the array will be given the value 0.00 and the two succeeding
elements will be given the values 1.00 and 2.00.

.. code-block:: fortran

  &MYNML
    X(1,1) = 0.00 , 1.00 , 2.00
  /

When writing a namelist, if no ``DELIM=`` is specified, by default a
double quote is used to delimit character strings. If -std=F95, F2003,
or F2008, etc, the delim status is set to 'none'.  Defaulting to
quotes ensures that namelists with character strings can be subsequently
read back in accurately.

.. _x-format-descriptor-without-count-field:

X format descriptor without count field
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To support legacy codes, GNU Fortran permits the count field of the
``X`` edit descriptor in ``FORMAT`` statements to be omitted.
When omitted, the count is implicitly assumed to be one.

.. code-block:: fortran

         PRINT 10, 2, 3
  10     FORMAT (I1, X, I1)

.. _commas-in-format-specifications:

Commas in FORMAT specifications
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To support legacy codes, GNU Fortran allows the comma separator
to be omitted immediately before and after character string edit
descriptors in ``FORMAT`` statements.  A comma with no following format
decriptor is permited if the :option:`-fdec-blank-format-item` is given on
the command line. This is considered non-conforming code and is
discouraged.

.. code-block:: fortran

         PRINT 10, 2, 3
  10     FORMAT ('FOO='I1' BAR='I2)
         print 20, 5, 6
  20     FORMAT (I3, I3,)

.. _missing-period-in-format-specifications:

Missing period in FORMAT specifications
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To support legacy codes, GNU Fortran allows missing periods in format
specifications if and only if :option:`-std=legacy` is given on the
command line.  This is considered non-conforming code and is
discouraged.

.. code-block:: fortran

         REAL :: value
         READ(*,10) value
  10     FORMAT ('F4')

.. _default-widths-for-f,-g-and-i-format-descriptors:

Default widths for F, G and I format descriptors
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To support legacy codes, GNU Fortran allows width to be omitted from format
specifications if and only if :option:`-fdec-format-defaults` is given on the
command line.  Default widths will be used. This is considered non-conforming
code and is discouraged.

.. code-block:: fortran

         REAL :: value1
         INTEGER :: value2
         WRITE(*,10) value1, value1, value2
  10     FORMAT ('F, G, I')

.. index:: I/O item lists

.. _i-o-item-lists:

I/O item lists
^^^^^^^^^^^^^^

To support legacy codes, GNU Fortran allows the input item list
of the ``READ`` statement, and the output item lists of the
``WRITE`` and ``PRINT`` statements, to start with a comma.

.. index:: Q exponent-letter

Q exponent-letter
^^^^^^^^^^^^^^^^^

GNU Fortran accepts real literal constants with an exponent-letter
of ``Q``, for example, ``1.23Q45``.  The constant is interpreted
as a ``REAL(16)`` entity on targets that support this type.  If
the target does not support ``REAL(16)`` but has a ``REAL(10)``
type, then the real-literal-constant will be interpreted as a
``REAL(10)`` entity.  In the absence of ``REAL(16)`` and
``REAL(10)``, an error will occur.

.. index:: BOZ literal constants

.. _boz-literal-constants:

BOZ literal constants
^^^^^^^^^^^^^^^^^^^^^

Besides decimal constants, Fortran also supports binary (``b``),
octal (``o``) and hexadecimal (``z``) integer constants.  The
syntax is: :samp:`prefix quote digits quote`, where the prefix is
either ``b``, ``o`` or ``z``, quote is either ``'`` or
``"`` and the digits are ``0`` or ``1`` for binary,
between ``0`` and ``7`` for octal, and between ``0`` and
``F`` for hexadecimal.  (Example: ``b'01011101'``.)

Up to Fortran 95, BOZ literal constants were only allowed to initialize
integer variables in DATA statements.  Since Fortran 2003 BOZ literal
constants are also allowed as actual arguments to the ``REAL``,
``DBLE``, ``INT`` and ``CMPLX`` intrinsic functions.
The BOZ literal constant is simply a string of bits, which is padded
or truncated as needed, during conversion to a numeric type.  The
Fortran standard states that the treatment of the sign bit is processor
dependent.  Gfortran interprets the sign bit as a user would expect.

As a deprecated extension, GNU Fortran allows hexadecimal BOZ literal
constants to be specified using the ``X`` prefix.  That the BOZ literal
constant can also be specified by adding a suffix to the string, for
example, ``Z'ABC'`` and ``'ABC'X`` are equivalent.  Additionally,
as extension, BOZ literals are permitted in some contexts outside of
``DATA`` and the intrinsic functions listed in the Fortran standard.
Use :option:`-fallow-invalid-boz` to enable the extension.

.. index:: array, indices of type real

.. _real-array-indices:

Real array indices
^^^^^^^^^^^^^^^^^^

As an extension, GNU Fortran allows the use of ``REAL`` expressions
or variables as array indices.

.. index:: operators, unary

.. _unary-operators:

Unary operators
^^^^^^^^^^^^^^^

As an extension, GNU Fortran allows unary plus and unary minus operators
to appear as the second operand of binary arithmetic operators without
the need for parenthesis.

.. code-block:: fortran

         X = Y * -Z

.. index:: conversion, to integer, conversion, to logical

.. _implicitly-convert-logical-and-integer-values:

Implicitly convert LOGICAL and INTEGER values
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As an extension for backwards compatibility with other compilers, GNU
Fortran allows the implicit conversion of ``LOGICAL`` values to
``INTEGER`` values and vice versa.  When converting from a
``LOGICAL`` to an ``INTEGER``, ``.FALSE.`` is interpreted as
zero, and ``.TRUE.`` is interpreted as one.  When converting from
``INTEGER`` to ``LOGICAL``, the value zero is interpreted as
``.FALSE.`` and any nonzero value is interpreted as ``.TRUE.``.

.. code-block:: fortran

          LOGICAL :: l
          l = 1

.. code-block:: fortran

          INTEGER :: i
          i = .TRUE.

However, there is no implicit conversion of ``INTEGER`` values in
``if`` -statements, nor of ``LOGICAL`` or ``INTEGER`` values
in I/O operations.

.. index:: Hollerith constants

.. _hollerith-constants-support:

Hollerith constants support
^^^^^^^^^^^^^^^^^^^^^^^^^^^

GNU Fortran supports Hollerith constants in assignments, ``DATA``
statements, function and subroutine arguments. A Hollerith constant is
written as a string of characters preceded by an integer constant
indicating the character count, and the letter ``H`` or
``h``, and stored in bytewise fashion in a numeric (``INTEGER``,
``REAL``, or ``COMPLEX``), ``LOGICAL`` or ``CHARACTER`` variable.
The constant will be padded with spaces or truncated to fit the size of
the variable in which it is stored.

Examples of valid uses of Hollerith constants:

.. code-block:: fortran

        complex*16 x(2)
        data x /16Habcdefghijklmnop, 16Hqrstuvwxyz012345/
        x(1) = 16HABCDEFGHIJKLMNOP
        call foo (4h abc)

Examples of Hollerith constants:

.. code-block:: fortran

        integer*4 a
        a = 0H         ! Invalid, at least one character is needed.
        a = 4HAB12     ! Valid
        a = 8H12345678 ! Valid, but the Hollerith constant will be truncated.
        a = 3Hxyz      ! Valid, but the Hollerith constant will be padded.

In general, Hollerith constants were used to provide a rudimentary
facility for handling character strings in early Fortran compilers,
prior to the introduction of ``CHARACTER`` variables in Fortran 77;
in those cases, the standard-compliant equivalent is to convert the
program to use proper character strings.  On occasion, there may be a
case where the intent is specifically to initialize a numeric variable
with a given byte sequence.  In these cases, the same result can be
obtained by using the ``TRANSFER`` statement, as in this example.

.. code-block:: fortran

        integer(kind=4) :: a
        a = transfer ("abcd", a)     ! equivalent to: a = 4Habcd

The use of the :option:`-fdec` option extends support of Hollerith constants
to comparisons:

.. code-block:: fortran

        integer*4 a
        a = 4hABCD
        if (a .ne. 4habcd) then
          write(*,*) "no match"
        end if

Supported types are numeric (``INTEGER``, ``REAL``, or ``COMPLEX``),
and ``CHARACTER``.

.. index:: conversion, to character

.. _character-conversion:

Character conversion
^^^^^^^^^^^^^^^^^^^^

Allowing character literals to be used in a similar way to Hollerith constants
is a non-standard extension.  This feature is enabled using
-fdec-char-conversions and only applies to character literals of ``kind=1``.

Character literals can be used in ``DATA`` statements and assignments with
numeric (``INTEGER``, ``REAL``, or ``COMPLEX``) or ``LOGICAL``
variables. Like Hollerith constants they are copied byte-wise fashion. The
constant will be padded with spaces or truncated to fit the size of the
variable in which it is stored.

Examples:

.. code-block:: fortran

        integer*4 x
        data x / 'abcd' /

        x = 'A'       ! Will be padded.
        x = 'ab1234'  ! Will be truncated.

.. index:: pointer, Cray

.. _cray-pointers:

Cray pointers
^^^^^^^^^^^^^

Cray pointers are part of a non-standard extension that provides a
C-like pointer in Fortran.  This is accomplished through a pair of
variables: an integer "pointer" that holds a memory address, and a
"pointee" that is used to dereference the pointer.

Pointer/pointee pairs are declared in statements of the form:

.. code-block:: fortran

          pointer ( <pointer> , <pointee> )

or,

.. code-block:: fortran

          pointer ( <pointer1> , <pointee1> ), ( <pointer2> , <pointee2> ), ...

The pointer is an integer that is intended to hold a memory address.
The pointee may be an array or scalar.
If an assumed-size array is permitted within the scoping unit, a
pointee can be an assumed-size array.
That is, the last dimension may be left unspecified by using a ``*``
in place of a value. A pointee cannot be an assumed shape array.
No space is allocated for the pointee.

The pointee may have its type declared before or after the pointer
statement, and its array specification (if any) may be declared
before, during, or after the pointer statement.  The pointer may be
declared as an integer prior to the pointer statement.  However, some
machines have default integer sizes that are different than the size
of a pointer, and so the following code is not portable:

.. code-block:: fortran

          integer ipt
          pointer (ipt, iarr)

If a pointer is declared with a kind that is too small, the compiler
will issue a warning; the resulting binary will probably not work
correctly, because the memory addresses stored in the pointers may be
truncated.  It is safer to omit the first line of the above example;
if explicit declaration of ipt's type is omitted, then the compiler
will ensure that ipt is an integer variable large enough to hold a
pointer.

Pointer arithmetic is valid with Cray pointers, but it is not the same
as C pointer arithmetic.  Cray pointers are just ordinary integers, so
the user is responsible for determining how many bytes to add to a
pointer in order to increment it.  Consider the following example:

.. code-block:: fortran

          real target(10)
          real pointee(10)
          pointer (ipt, pointee)
          ipt = loc (target)
          ipt = ipt + 1

The last statement does not set ``ipt`` to the address of
``target(1)``, as it would in C pointer arithmetic.  Adding ``1``
to ``ipt`` just adds one byte to the address stored in ``ipt``.

Any expression involving the pointee will be translated to use the
value stored in the pointer as the base address.

To get the address of elements, this extension provides an intrinsic
function ``LOC()``.  The ``LOC()`` function is equivalent to the
``&`` operator in C, except the address is cast to an integer type:

.. code-block:: fortran

          real ar(10)
          pointer(ipt, arpte(10))
          real arpte
          ipt = loc(ar)  ! Makes arpte is an alias for ar
          arpte(1) = 1.0 ! Sets ar(1) to 1.0

The pointer can also be set by a call to the ``MALLOC`` intrinsic
(see :ref:`MALLOC`).

Cray pointees often are used to alias an existing variable.  For
example:

.. code-block:: fortran

          integer target(10)
          integer iarr(10)
          pointer (ipt, iarr)
          ipt = loc(target)

As long as ``ipt`` remains unchanged, ``iarr`` is now an alias for
``target``.  The optimizer, however, will not detect this aliasing, so
it is unsafe to use ``iarr`` and ``target`` simultaneously.  Using
a pointee in any way that violates the Fortran aliasing rules or
assumptions is illegal.  It is the user's responsibility to avoid doing
this; the compiler works under the assumption that no such aliasing
occurs.

Cray pointers will work correctly when there is no aliasing (i.e., when
they are used to access a dynamically allocated block of memory), and
also in any routine where a pointee is used, but any variable with which
it shares storage is not used.  Code that violates these rules may not
run as the user intends.  This is not a bug in the optimizer; any code
that violates the aliasing rules is illegal.  (Note that this is not
unique to GNU Fortran; any Fortran compiler that supports Cray pointers
will 'incorrectly' optimize code with illegal aliasing.)

There are a number of restrictions on the attributes that can be applied
to Cray pointers and pointees.  Pointees may not have the
``ALLOCATABLE``, ``INTENT``, ``OPTIONAL``, ``DUMMY``,
``TARGET``, ``INTRINSIC``, or ``POINTER`` attributes.  Pointers
may not have the ``DIMENSION``, ``POINTER``, ``TARGET``,
``ALLOCATABLE``, ``EXTERNAL``, or ``INTRINSIC`` attributes, nor
may they be function results.  Pointees may not occur in more than one
pointer statement.  A pointee cannot be a pointer.  Pointees cannot occur
in equivalence, common, or data statements.

A Cray pointer may also point to a function or a subroutine.  For
example, the following excerpt is valid:

.. code-block:: fortran

    implicit none
    external sub
    pointer (subptr,subpte)
    external subpte
    subptr = loc(sub)
    call subpte()
    [...]
    subroutine sub
    [...]
    end subroutine sub

A pointer may be modified during the course of a program, and this
will change the location to which the pointee refers.  However, when
pointees are passed as arguments, they are treated as ordinary
variables in the invoked function.  Subsequent changes to the pointer
will not change the base address of the array that was passed.

.. index:: CONVERT specifier

.. _convert-specifier:

CONVERT specifier
^^^^^^^^^^^^^^^^^

GNU Fortran allows the conversion of unformatted data between little-
and big-endian representation to facilitate moving of data
between different systems.  The conversion can be indicated with
the ``CONVERT`` specifier on the ``OPEN`` statement.
See :ref:`gfortran_convert_unit`, for an alternative way of specifying
the data format via an environment variable.

Valid values for ``CONVERT`` on most systems are:

* ``CONVERT='NATIVE'`` Use the native format.  This is the default.

* ``CONVERT='SWAP'`` Swap between little- and big-endian.

* ``CONVERT='LITTLE_ENDIAN'`` Use the little-endian representation
  for unformatted files.

* ``CONVERT='BIG_ENDIAN'`` Use the big-endian representation for
  unformatted files.

On POWER systems which support :option:`-mabi=ieeelongdouble`,
there are additional options, which can be combined with the others
with commas. Those are

* ``CONVERT='R16_IEEE'`` Use IEEE 128-bit format for
  ``REAL(KIND=16)``.

* ``CONVERT='R16_IBM'`` Use IBM ``long double`` format for
  real ``REAL(KIND=16)``.

Using the option could look like this:

.. code-block:: fortran

    open(file='big.dat',form='unformatted',access='sequential', &
         convert='big_endian')

The value of the conversion can be queried by using
``INQUIRE(CONVERT=ch)``.  The values returned are
``'BIG_ENDIAN'`` and ``'LITTLE_ENDIAN'``.

``CONVERT`` works between big- and little-endian for
``INTEGER`` values of all supported kinds and for ``REAL``
on IEEE systems of kinds 4 and 8.  Conversion between different
'extended double' types on different architectures such as
m68k and x86_64, which GNU Fortran
supports as ``REAL(KIND=10)`` and ``REAL(KIND=16)``, will
probably not work.

*Note that the values specified via the GFORTRAN_CONVERT_UNIT
environment variable will override the CONVERT specifier in the
open statement*.  This is to give control over data formats to
users who do not have the source code of their program available.

Using anything but the native representation for unformatted data
carries a significant speed overhead.  If speed in this area matters
to you, it is best if you use this only for data that needs to be
portable.

.. index:: OpenMP

.. _openmp:

OpenMP
^^^^^^

OpenMP (Open Multi-Processing) is an application programming
interface (API) that supports multi-platform shared memory
multiprocessing programming in C/C++ and Fortran on many
architectures, including Unix and Microsoft Windows platforms.
It consists of a set of compiler directives, library routines,
and environment variables that influence run-time behavior.

GNU Fortran strives to be compatible to the
`OpenMP Application Program Interface v4.5 <https://openmp.org/specifications/>`_.

To enable the processing of the OpenMP directive ``!$omp`` in
free-form source code; the ``c$omp``, ``*$omp`` and ``!$omp``
directives in fixed form; the ``!$`` conditional compilation sentinels
in free form; and the ``c$``, ``*$`` and ``!$`` sentinels
in fixed form, :command:`gfortran` needs to be invoked with the
:option:`-fopenmp`.  This also arranges for automatic linking of the
GNU Offloading and Multi Processing Runtime Library
:ref:`libgomp:top`.

The OpenMP Fortran runtime library routines are provided both in a
form of a Fortran 90 module named ``omp_lib`` and in a form of
a Fortran ``include`` file named :samp:`omp_lib.h`.

An example of a parallelized loop taken from Appendix A.1 of
the OpenMP Application Program Interface v2.5:

.. code-block:: fortran

  SUBROUTINE A1(N, A, B)
    INTEGER I, N
    REAL B(N), A(N)
  !$OMP PARALLEL DO !I is private by default
    DO I=2,N
      B(I) = (A(I) + A(I-1)) / 2.0
    ENDDO
  !$OMP END PARALLEL DO
  END SUBROUTINE A1

.. note::

  :option:`-fopenmp` implies :option:`-frecursive`, i.e., all local arrays
  will be allocated on the stack.  When porting existing code to OpenMP,
  this may lead to surprising results, especially to segmentation faults
  if the stacksize is limited.

.. note::

  On glibc-based systems, OpenMP enabled applications cannot be statically
  linked due to limitations of the underlying pthreads-implementation.  It
  might be possible to get a working solution if
  :command:`-Wl,--whole-archive -lpthread -Wl,--no-whole-archive` is added
  to the command line.  However, this is not supported by :command:`gcc` and
  thus not recommended.

.. index:: OpenACC

.. _openacc:

OpenACC
^^^^^^^

OpenACC is an application programming interface (API) that supports
offloading of code to accelerator devices.  It consists of a set of
compiler directives, library routines, and environment variables that
influence run-time behavior.

GNU Fortran strives to be compatible to the
`OpenACC Application Programming
Interface v2.6 <https://www.openacc.org/>`_.

To enable the processing of the OpenACC directive ``!$acc`` in
free-form source code; the ``c$acc``, ``*$acc`` and ``!$acc``
directives in fixed form; the ``!$`` conditional compilation
sentinels in free form; and the ``c$``, ``*$`` and ``!$``
sentinels in fixed form, :command:`gfortran` needs to be invoked with
the :option:`-fopenacc`.  This also arranges for automatic linking of
the GNU Offloading and Multi Processing Runtime Library
:ref:`libgomp:top`.

The OpenACC Fortran runtime library routines are provided both in a
form of a Fortran 90 module named ``openacc`` and in a form of a
Fortran ``include`` file named :samp:`openacc_lib.h`.

.. index:: argument list functions, %VAL, %REF, %LOC

.. _argument-list-functions:

Argument list functions %VAL, %REF and %LOC
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GNU Fortran supports argument list functions ``%VAL``, ``%REF``
and ``%LOC`` statements, for backward compatibility with g77.
It is recommended that these should be used only for code that is
accessing facilities outside of GNU Fortran, such as operating system
or windowing facilities.  It is best to constrain such uses to isolated
portions of a program--portions that deal specifically and exclusively
with low-level, system-dependent facilities.  Such portions might well
provide a portable interface for use by the program as a whole, but are
themselves not portable, and should be thoroughly tested each time they
are rebuilt using a new compiler or version of a compiler.

``%VAL`` passes a scalar argument by value, ``%REF`` passes it by
reference and ``%LOC`` passes its memory location.  Since gfortran
already passes scalar arguments by reference, ``%REF`` is in effect
a do-nothing.  ``%LOC`` has the same effect as a Fortran pointer.

An example of passing an argument by value to a C subroutine foo.:

.. code-block:: fortran

  C
  C prototype      void foo_ (float x);
  C
        external foo
        real*4 x
        x = 3.14159
        call foo (%VAL (x))
        end

For details refer to the g77 manual
https://gcc.gnu.org/onlinedocs/gcc-3.4.6/g77/index.html#Top.

Also, ``c_by_val.f`` and its partner ``c_by_val.c`` of the
GNU Fortran testsuite are worth a look.

.. index:: EOF, BACKSPACE, REWIND

.. _read-write-after-eof-marker:

Read/Write after EOF marker
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Some legacy codes rely on allowing ``READ`` or ``WRITE`` after the
EOF file marker in order to find the end of a file. GNU Fortran normally
rejects these codes with a run-time error message and suggests the user
consider ``BACKSPACE`` or ``REWIND`` to properly position
the file before the EOF marker.  As an extension, the run-time error may
be disabled using -std=legacy.

.. index:: STRUCTURE, RECORD

.. _structure-and-record:

STRUCTURE and RECORD
^^^^^^^^^^^^^^^^^^^^

Record structures are a pre-Fortran-90 vendor extension to create
user-defined aggregate data types.  Support for record structures in GNU
Fortran can be enabled with the :option:`-fdec-structure` compile flag.
If you have a choice, you should instead use Fortran 90's 'derived types',
which have a different syntax.

In many cases, record structures can easily be converted to derived types.
To convert, replace  ``STRUCTURE /``:samp:`{structure-name}` ``/``
by ``TYPE`` :samp:`{type-name}`.  Additionally, replace
``RECORD /``:samp:`{structure-name}` ``/`` by
``TYPE(``:samp:`{type-name}` ``)``. Finally, in the component access,
replace the period (``.``) by the percent sign (``%``).

Here is an example of code using the non portable record structure syntax:

.. code-block:: fortran

  ! Declaring a structure named ``item'' and containing three fields:
  ! an integer ID, an description string and a floating-point price.
  STRUCTURE /item/
    INTEGER id
    CHARACTER(LEN=200) description
    REAL price
  END STRUCTURE

  ! Define two variables, an single record of type ``item''
  ! named ``pear'', and an array of items named ``store_catalog''
  RECORD /item/ pear, store_catalog(100)

  ! We can directly access the fields of both variables
  pear.id = 92316
  pear.description = "juicy D'Anjou pear"
  pear.price = 0.15
  store_catalog(7).id = 7831
  store_catalog(7).description = "milk bottle"
  store_catalog(7).price = 1.2

  ! We can also manipulate the whole structure
  store_catalog(12) = pear
  print *, store_catalog(12)

This code can easily be rewritten in the Fortran 90 syntax as following:

.. code-block:: fortran

  ! ``STRUCTURE /name/ ... END STRUCTURE'' becomes
  ! ``TYPE name ... END TYPE''
  TYPE item
    INTEGER id
    CHARACTER(LEN=200) description
    REAL price
  END TYPE

  ! ``RECORD /name/ variable'' becomes ``TYPE(name) variable''
  TYPE(item) pear, store_catalog(100)

  ! Instead of using a dot (.) to access fields of a record, the
  ! standard syntax uses a percent sign (%)
  pear%id = 92316
  pear%description = "juicy D'Anjou pear"
  pear%price = 0.15
  store_catalog(7)%id = 7831
  store_catalog(7)%description = "milk bottle"
  store_catalog(7)%price = 1.2

  ! Assignments of a whole variable do not change
  store_catalog(12) = pear
  print *, store_catalog(12)

GNU Fortran implements STRUCTURES like derived types with the following
rules and exceptions:

* Structures act like derived types with the ``SEQUENCE`` attribute.
  Otherwise they may contain no specifiers.

* Structures may contain a special field with the name ``%FILL``.
  This will create an anonymous component which cannot be accessed but occupies
  space just as if a component of the same type was declared in its place, useful
  for alignment purposes.  As an example, the following structure will consist
  of at least sixteen bytes:

  .. code-block:: fortran

    structure /padded/
      character(4) start
      character(8) %FILL
      character(4) end
    end structure

* Structures may share names with other symbols. For example, the following
  is invalid for derived types, but valid for structures:

  .. code-block:: fortran

    structure /header/
      ! ...
    end structure
    record /header/ header

* Structure types may be declared nested within another parent structure.
  The syntax is:

  .. code-block:: fortran

    structure /type-name/
        ...
        structure [/<type-name>/] <field-list>
    ...

  The type name may be ommitted, in which case the structure type itself is
  anonymous, and other structures of the same type cannot be instantiated. The
  following shows some examples:

  .. code-block:: fortran

    structure /appointment/
      ! nested structure definition: app_time is an array of two 'time'
      structure /time/ app_time (2)
        integer(1) hour, minute
      end structure
      character(10) memo
    end structure

    ! The 'time' structure is still usable
    record /time/ now
    now = time(5, 30)

    ...

    structure /appointment/
      ! anonymous nested structure definition
      structure start, end
        integer(1) hour, minute
      end structure
      character(10) memo
    end structure

* Structures may contain ``UNION`` blocks. For more detail see the
  section on :ref:`union-and-map`.

* Structures support old-style initialization of components, like
  those described in :ref:`old-style-variable-initialization`. For array
  initializers, an initializer may contain a repeat specification of the form
  ``<literal-integer> * <constant-initializer>``. The value of the integer
  indicates the number of times to repeat the constant initializer when expanding
  the initializer list.

.. index:: UNION, MAP

.. _union-and-map:

UNION and MAP
^^^^^^^^^^^^^

Unions are an old vendor extension which were commonly used with the
non-standard :ref:`structure-and-record` extensions. Use of ``UNION`` and
``MAP`` is automatically enabled with :option:`-fdec-structure`.

A ``UNION`` declaration occurs within a structure; within the definition of
each union is a number of ``MAP`` blocks. Each ``MAP`` shares storage
with its sibling maps (in the same union), and the size of the union is the
size of the largest map within it, just as with unions in C. The major
difference is that component references do not indicate which union or map the
component is in (the compiler gets to figure that out).

Here is a small example:

.. code-block:: fortran

  structure /myunion/
  union
    map
      character(2) w0, w1, w2
    end map
    map
      character(6) long
    end map
  end union
  end structure

  record /myunion/ rec
  ! After this assignment...
  rec.long = 'hello!'

  ! The following is true:
  ! rec.w0 === 'he'
  ! rec.w1 === 'll'
  ! rec.w2 === 'o!'

The two maps share memory, and the size of the union is ultimately six bytes:

.. code-block::

  0    1    2    3    4   5   6     Byte offset
  -------------------------------
  |    |    |    |    |    |    |
  -------------------------------

  ^    W0   ^    W1   ^    W2   ^
   \-------/ \-------/ \-------/

  ^             LONG            ^
   \---------------------------/

Following is an example mirroring the layout of an Intel x86_64 register:

.. code-block:: fortran

  structure /reg/
    union ! U0                ! rax
      map
        character(16) rx
      end map
      map
        character(8) rh         ! rah
        union ! U1
          map
            character(8) rl     ! ral
          end map
          map
            character(8) ex     ! eax
          end map
          map
            character(4) eh     ! eah
            union ! U2
              map
                character(4) el ! eal
              end map
              map
                character(4) x  ! ax
              end map
              map
                character(2) h  ! ah
                character(2) l  ! al
              end map
            end union
          end map
        end union
      end map
    end union
  end structure
  record /reg/ a

  ! After this assignment...
  a.rx     =     'AAAAAAAA.BBB.C.D'

  ! The following is true:
  a.rx === 'AAAAAAAA.BBB.C.D'
  a.rh === 'AAAAAAAA'
  a.rl ===         '.BBB.C.D'
  a.ex ===         '.BBB.C.D'
  a.eh ===         '.BBB'
  a.el ===             '.C.D'
  a.x  ===             '.C.D'
  a.h  ===             '.C'
  a.l  ===               '.D'

.. index:: intrinsics, integer

.. _type-variants-for-integer-intrinsics:

Type variants for integer intrinsics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Similar to the D/C prefixes to real functions to specify the input/output
types, GNU Fortran offers B/I/J/K prefixes to integer functions for
compatibility with DEC programs. The types implied by each are:

.. code-block:: fortran

  B - INTEGER(kind=1)
  I - INTEGER(kind=2)
  J - INTEGER(kind=4)
  K - INTEGER(kind=8)

GNU Fortran supports these with the flag :option:`-fdec-intrinsic-ints`.
Intrinsics for which prefixed versions are available and in what form are noted
in :ref:`intrinsic-procedures`. The complete list of supported intrinsics is
here:

.. list-table::
   :header-rows: 1

   * - Intrinsic
     - B
     - I
     - J
     - K

   * - ``ABS``
     - ``BABS``
     - ``IIABS``
     - ``JIABS``
     - ``KIABS``
   * - ``BTEST``
     - ``BBTEST``
     - ``BITEST``
     - ``BJTEST``
     - ``BKTEST``
   * - ``IAND``
     - ``BIAND``
     - ``IIAND``
     - ``JIAND``
     - ``KIAND``
   * - ``IBCLR``
     - ``BBCLR``
     - ``IIBCLR``
     - ``JIBCLR``
     - ``KIBCLR``
   * - ``IBITS``
     - ``BBITS``
     - ``IIBITS``
     - ``JIBITS``
     - ``KIBITS``
   * - ``IBSET``
     - ``BBSET``
     - ``IIBSET``
     - ``JIBSET``
     - ``KIBSET``
   * - ``IEOR``
     - ``BIEOR``
     - ``IIEOR``
     - ``JIEOR``
     - ``KIEOR``
   * - ``IOR``
     - ``BIOR``
     - ``IIOR``
     - ``JIOR``
     - ``KIOR``
   * - ``ISHFT``
     - ``BSHFT``
     - ``IISHFT``
     - ``JISHFT``
     - ``KISHFT``
   * - ``ISHFTC``
     - ``BSHFTC``
     - ``IISHFTC``
     - ``JISHFTC``
     - ``KISHFTC``
   * - ``MOD``
     - ``BMOD``
     - ``IMOD``
     - ``JMOD``
     - ``KMOD``
   * - ``NOT``
     - ``BNOT``
     - ``INOT``
     - ``JNOT``
     - ``KNOT``
   * - ``REAL``
     - ``--``
     - ``FLOATI``
     - ``FLOATJ``
     - ``FLOATK``

.. _automatic-and-static-attributes:

AUTOMATIC and STATIC attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

With :option:`-fdec-static` GNU Fortran supports the DEC extended attributes
``STATIC`` and ``AUTOMATIC`` to provide explicit specification of entity
storage.  These follow the syntax of the Fortran standard ``SAVE`` attribute.

``STATIC`` is exactly equivalent to ``SAVE``, and specifies that
an entity should be allocated in static memory.  As an example, ``STATIC``
local variables will retain their values across multiple calls to a function.

Entities marked ``AUTOMATIC`` will be stack automatic whenever possible.
``AUTOMATIC`` is the default for local variables smaller than
:option:`-fmax-stack-var-size`, unless :option:`-fno-automatic` is given.  This
attribute overrides :option:`-fno-automatic`, :option:`-fmax-stack-var-size`, and
blanket ``SAVE`` statements.

Examples:

.. code-block:: fortran

  subroutine f
    integer, automatic :: i  ! automatic variable
    integer x, y             ! static variables
    save
    ...
  endsubroutine

.. code-block:: fortran

  subroutine f
    integer a, b, c, x, y, z
    static :: x
    save y
    automatic z, c
    ! a, b, c, and z are automatic
    ! x and y are static
  endsubroutine

.. code-block:: fortran

  ! Compiled with -fno-automatic
  subroutine f
    integer a, b, c, d
    automatic :: a
    ! a is automatic; b, c, and d are static
  endsubroutine

.. index:: intrinsics, math, intrinsics, trigonometric functions

.. _extended-math-intrinsics:

Extended math intrinsics
^^^^^^^^^^^^^^^^^^^^^^^^

GNU Fortran supports an extended list of mathematical intrinsics with the
compile flag :option:`-fdec-math` for compatability with legacy code.
These intrinsics are described fully in :ref:`intrinsic-procedures` where it is
noted that they are extensions and should be avoided whenever possible.

Specifically, :option:`-fdec-math` enables the :ref:`COTAN` intrinsic, and
trigonometric intrinsics which accept or produce values in degrees instead of
radians.  Here is a summary of the new intrinsics:

.. list-table::
   :header-rows: 1

   * - Radians
     - Degrees

   * - ``ACOS``
     - ``ACOSD`` \*
   * - ``ASIN``
     - ``ASIND`` \*
   * - ``ATAN``
     - ``ATAND`` \*
   * - ``ATAN2``
     - ``ATAN2D`` \*
   * - ``COS``
     - ``COSD`` \*
   * - ``COTAN`` \*
     - ``COTAND`` \*
   * - ``SIN``
     - ``SIND`` \*
   * - ``TAN``
     - ``TAND`` \*

\* Enabled with :option:`-fdec-math`.

For advanced users, it may be important to know the implementation of these
functions. They are simply wrappers around the standard radian functions, which
have more accurate builtin versions. These functions convert their arguments
(or results) to degrees (or radians) by taking the value modulus 360 (or 2\*pi)
and then multiplying it by a constant radian-to-degree (or degree-to-radian)
factor, as appropriate. The factor is computed at compile-time as 180/pi (or
pi/180).

.. index:: form feed whitespace

.. _form-feed-as-whitespace:

Form feed as whitespace
^^^^^^^^^^^^^^^^^^^^^^^

Historically, legacy compilers allowed insertion of form feed characters ('\f',
ASCII 0xC) at the beginning of lines for formatted output to line printers,
though the Fortran standard does not mention this. GNU Fortran supports the
interpretation of form feed characters in source as whitespace for
compatibility.

.. index:: type alias print

.. _type-as-an-alias-for-print:

TYPE as an alias for PRINT
^^^^^^^^^^^^^^^^^^^^^^^^^^

For compatibility, GNU Fortran will interpret ``TYPE`` statements as
``PRINT`` statements with the flag :option:`-fdec`.  With this flag asserted,
the following two examples are equivalent:

.. code-block:: fortran

  TYPE *, 'hello world'

.. code-block:: fortran

  PRINT *, 'hello world'

.. index:: LOC

.. _%loc-as-an-rvalue:

%LOC as an rvalue
^^^^^^^^^^^^^^^^^

Normally ``%LOC`` is allowed only in parameter lists.  However the intrinsic
function ``LOC`` does the same thing, and is usable as the right-hand-side of
assignments. For compatibility, GNU Fortran supports the use of ``%LOC`` as
an alias for the builtin ``LOC`` with :option:`-std=legacy`.  With this
feature enabled the following two examples are equivalent:

.. code-block:: fortran

  integer :: i, l
  l = %loc(i)
  call sub(l)

.. code-block:: fortran

  integer :: i
  call sub(%loc(i))

.. index:: operators, xor

.. _.xor.-operator:

.XOR. operator
^^^^^^^^^^^^^^

GNU Fortran supports ``.XOR.`` as a logical operator with ``-std=legacy``
for compatibility with legacy code. ``.XOR.`` is equivalent to
``.NEQV.``. That is, the output is true if and only if the inputs differ.

.. index:: logical, bitwise

.. _bitwise-logical-operators:

Bitwise logical operators
^^^^^^^^^^^^^^^^^^^^^^^^^

With :option:`-fdec`, GNU Fortran relaxes the type constraints on
logical operators to allow integer operands, and performs the corresponding
bitwise operation instead.  This flag is for compatibility only, and should be
avoided in new code.  Consider:

.. code-block:: fortran

    INTEGER :: i, j
    i = z'33'
    j = z'cc'
    print *, i .AND. j

In this example, compiled with :option:`-fdec`, GNU Fortran will
replace the ``.AND.`` operation with a call to the intrinsic
function, yielding the bitwise-and of ``i`` and ``j``.

Note that this conversion will occur if at least one operand is of integral
type.  As a result, a logical operand will be converted to an integer when the
other operand is an integer in a logical operation.  In this case,
``.TRUE.`` is converted to ``1`` and ``.FALSE.`` to ``0``.

Here is the mapping of logical operator to bitwise intrinsic used with
:option:`-fdec` :

.. list-table::
   :header-rows: 1

   * - Operator
     - Intrinsic
     - Bitwise operation

   * - ``.NOT.``
     - ``NOT``
     - complement
   * - ``.AND.``
     - ``IAND``
     - intersection
   * - ``.OR.``
     - ``IOR``
     - union
   * - ``.NEQV.``
     - ``IEOR``
     - exclusive or
   * - ``.EQV.``
     - ``NOT(IEOR)``
     - complement of exclusive or

.. _extended-i-o-specifiers:

Extended I/O specifiers
^^^^^^^^^^^^^^^^^^^^^^^

GNU Fortran supports the additional legacy I/O specifiers
``CARRIAGECONTROL``, ``READONLY``, and ``SHARE`` with the
compile flag :option:`-fdec`, for compatibility.

.. envvar:: CARRIAGECONTROL

  The ``CARRIAGECONTROL`` specifier allows a user to control line
  termination settings between output records for an I/O unit. The specifier has
  no meaning for readonly files. When ``CARRAIGECONTROL`` is specified upon
  opening a unit for formatted writing, the exact ``CARRIAGECONTROL`` setting
  determines what characters to write between output records. The syntax is:

  .. code-block:: fortran

    OPEN(..., CARRIAGECONTROL=cc)

  Where *cc* is a character expression that evaluates to one of the
  following values:

  .. list-table::

     * - ``'LIST'``
       - One line feed between records (default)
     * - ``'FORTRAN'``
       - Legacy interpretation of the first character (see below)
     * - ``'NONE'``
       - No separator between records

  With ``CARRIAGECONTROL='FORTRAN'``, when a record is written, the first
  character of the input record is not written, and instead determines the output
  record separator as follows:

  .. list-table::
     :header-rows: 1

     * - Leading character
       - Meaning
       - Output separating character(s)

     * - ``'+'``
       - Overprinting
       - Carriage return only
     * - ``'-'``
       - New line
       - Line feed and carriage return
     * - ``'0'``
       - Skip line
       - Two line feeds and carriage return
     * - ``'1'``
       - New page
       - Form feed and carriage return
     * - ``'$'``
       - Prompting
       - Line feed (no carriage return)
     * - ``CHAR(0)``
       - Overprinting (no advance)
       - None

.. envvar:: READONLY

  The ``READONLY`` specifier may be given upon opening a unit, and is
  equivalent to specifying ``ACTION='READ'``, except that the file may not be
  deleted on close (i.e. ``CLOSE`` with ``STATUS="DELETE"``). The syntax
  is:

  .. code-block:: fortran

    OPEN(..., READONLY)

.. envvar:: SHARE

  The ``SHARE`` specifier allows system-level locking on a unit upon opening
  it for controlled access from multiple processes/threads. The ``SHARE``
  specifier has several forms:

  .. code-block:: fortran

    OPEN(..., SHARE=sh)
    OPEN(..., SHARED)
    OPEN(..., NOSHARED)

  Where *sh* in the first form is a character expression that evaluates to
  a value as seen in the table below. The latter two forms are aliases
  for particular values of *sh*:

  .. list-table::
     :header-rows: 1

     * - Explicit form
       - Short form
       - Meaning

     * - ``SHARE='DENYRW'``
       - ``NOSHARED``
       - Exclusive (write) lock
     * - ``SHARE='DENYNONE'``
       - ``SHARED``
       - Shared (read) lock

  In general only one process may hold an exclusive (write) lock for a given file
  at a time, whereas many processes may hold shared (read) locks for the same
  file.

  The behavior of locking may vary with your operating system. On POSIX systems,
  locking is implemented with ``fcntl``. Consult your corresponding operating
  system's manual pages for further details. Locking via ``SHARE=`` is not
  supported on other systems.

.. index:: PARAMETER

.. _legacy-parameter-statements:

Legacy PARAMETER statements
^^^^^^^^^^^^^^^^^^^^^^^^^^^

For compatibility, GNU Fortran supports legacy PARAMETER statements without
parentheses with :option:`-std=legacy`.  A warning is emitted if used with
:option:`-std=gnu`, and an error is acknowledged with a real Fortran standard
flag (:option:`-std=f95`, etc...).  These statements take the following form:

.. code-block:: fortran

  implicit real (E)
  parameter e = 2.718282
  real c
  parameter c = 3.0e8

.. index:: exponent

.. _default-exponents:

Default exponents
^^^^^^^^^^^^^^^^^

For compatibility, GNU Fortran supports a default exponent of zero in real
constants with :option:`-fdec`.  For example, ``9e`` would be
interpreted as ``9e0``, rather than an error.