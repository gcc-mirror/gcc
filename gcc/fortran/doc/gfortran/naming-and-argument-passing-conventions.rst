..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _naming-and-argument-passing-conventions:

Naming and argument-passing conventions
***************************************

This section gives an overview about the naming convention of procedures
and global variables and about the argument passing conventions used by
GNU Fortran.  If a C binding has been specified, the naming convention
and some of the argument-passing conventions change.  If possible,
mixed-language and mixed-compiler projects should use the better defined
C binding for interoperability.  See see :ref:`interoperability-with-c`.

.. toctree::
  :maxdepth: 2


.. _naming-conventions:

Naming conventions
^^^^^^^^^^^^^^^^^^

According the Fortran standard, valid Fortran names consist of a letter
between ``A`` to ``Z``, ``a`` to ``z``, digits ``0``,
``1`` to ``9`` and underscores (``_``) with the restriction
that names may only start with a letter.  As vendor extension, the
dollar sign (``$``) is additionally permitted with the option
:option:`-fdollar-ok`, but not as first character and only if the
target system supports it.

By default, the procedure name is the lower-cased Fortran name with an
appended underscore (``_``); using :option:`-fno-underscoring` no
underscore is appended while ``-fsecond-underscore`` appends two
underscores.  Depending on the target system and the calling convention,
the procedure might be additionally dressed; for instance, on 32bit
Windows with ``stdcall``, an at-sign ``@`` followed by an integer
number is appended.  For the changing the calling convention, see
see :ref:`gnu-fortran-compiler-directives`.

For common blocks, the same convention is used, i.e. by default an
underscore is appended to the lower-cased Fortran name.  Blank commons
have the name ``__BLNK__``.

For procedures and variables declared in the specification space of a
module, the name is formed by ``__``, followed by the lower-cased
module name, ``_MOD_``, and the lower-cased Fortran name.  Note that
no underscore is appended.

.. _argument-passing-conventions:

Argument passing conventions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Subroutines do not return a value (matching C99's ``void``) while
functions either return a value as specified in the platform ABI or
the result variable is passed as hidden argument to the function and
no result is returned.  A hidden result variable is used when the
result variable is an array or of type ``CHARACTER``.

Arguments are passed according to the platform ABI. In particular,
complex arguments might not be compatible to a struct with two real
components for the real and imaginary part. The argument passing
matches the one of C99's ``_Complex``.  Functions with scalar
complex result variables return their value and do not use a
by-reference argument.  Note that with the :option:`-ff2c` option,
the argument passing is modified and no longer completely matches
the platform ABI.  Some other Fortran compilers use ``f2c``
semantic by default; this might cause problems with
interoperablility.

GNU Fortran passes most arguments by reference, i.e. by passing a
pointer to the data.  Note that the compiler might use a temporary
variable into which the actual argument has been copied, if required
semantically (copy-in/copy-out).

For arguments with ``ALLOCATABLE`` and ``POINTER``
attribute (including procedure pointers), a pointer to the pointer
is passed such that the pointer address can be modified in the
procedure.

For dummy arguments with the ``VALUE`` attribute: Scalar arguments
of the type ``INTEGER``, ``LOGICAL``, ``REAL`` and
``COMPLEX`` are passed by value according to the platform ABI.
(As vendor extension and not recommended, using ``%VAL()`` in the
call to a procedure has the same effect.) For ``TYPE(C_PTR)`` and
procedure pointers, the pointer itself is passed such that it can be
modified without affecting the caller.

.. todo:: Document how VALUE is handled for CHARACTER, TYPE,
  CLASS and arrays, i.e. whether the copy-in is done in the caller
  or in the callee.

For Boolean (``LOGICAL``) arguments, please note that GCC expects
only the integer value 0 and 1.  If a GNU Fortran ``LOGICAL``
variable contains another integer value, the result is undefined.
As some other Fortran compilers use -1 for ``.TRUE.``,
extra care has to be taken -- such as passing the value as
``INTEGER``.  (The same value restriction also applies to other
front ends of GCC, e.g. to GCC's C99 compiler for ``_Bool``
or GCC's Ada compiler for ``Boolean``.)

For arguments of ``CHARACTER`` type, the character length is passed
as a hidden argument at the end of the argument list.  For
deferred-length strings, the value is passed by reference, otherwise
by value.  The character length has the C type ``size_t`` (or
``INTEGER(kind=C_SIZE_T)`` in Fortran).  Note that this is
different to older versions of the GNU Fortran compiler, where the
type of the hidden character length argument was a C ``int``.  In
order to retain compatibility with older versions, one can e.g. for
the following Fortran procedure

.. code-block:: fortran

  subroutine fstrlen (s, a)
     character(len=*) :: s
     integer :: a
     print*, len(s)
  end subroutine fstrlen

define the corresponding C prototype as follows:

.. code-block:: fortran

  #if __GNUC__ > 7
  typedef size_t fortran_charlen_t;
  #else
  typedef int fortran_charlen_t;
  #endif

  void fstrlen_ (char*, int*, fortran_charlen_t);

In order to avoid such compiler-specific details, for new code it is
instead recommended to use the ISO_C_BINDING feature.

Note with C binding, ``CHARACTER(len=1)`` result variables are
returned according to the platform ABI and no hidden length argument
is used for dummy arguments; with ``VALUE``, those variables are
passed by value.

For ``OPTIONAL`` dummy arguments, an absent argument is denoted
by a NULL pointer, except for scalar dummy arguments of intrinsic type
which have the ``VALUE`` attribute.  For those, a hidden Boolean
argument (``logical(kind=C_bool),value``) is used to indicate
whether the argument is present.

Arguments which are assumed-shape, assumed-rank or deferred-rank
arrays or, with :option:`-fcoarray=lib`, allocatable scalar coarrays use
an array descriptor.  All other arrays pass the address of the
first element of the array.  With :option:`-fcoarray=lib`, the token
and the offset belonging to nonallocatable coarrays dummy arguments
are passed as hidden argument along the character length hidden
arguments.  The token is an opaque pointer identifying the coarray
and the offset is a passed-by-value integer of kind ``C_PTRDIFF_T``,
denoting the byte offset between the base address of the coarray and
the passed scalar or first element of the passed array.

The arguments are passed in the following order

* Result variable, when the function result is passed by reference

* Character length of the function result, if it is a of type
  ``CHARACTER`` and no C binding is used

* The arguments in the order in which they appear in the Fortran
  declaration

* The present status for optional arguments with value attribute,
  which are internally passed by value

* The character length and/or coarray token and offset for the first
  argument which is a ``CHARACTER`` or a nonallocatable coarray dummy
  argument, followed by the hidden arguments of the next dummy argument
  of such a type
