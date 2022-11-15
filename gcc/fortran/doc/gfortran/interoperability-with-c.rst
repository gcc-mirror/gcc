..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: interoperability with C, C interoperability

.. _interoperability-with-c:

Interoperability with C
***********************

.. toctree::
  :maxdepth: 2

Since Fortran 2003 (ISO/IEC 1539-1:2004(E)) there is a
standardized way to generate procedure and derived-type
declarations and global variables that are interoperable with C
(ISO/IEC 9899:1999).  The ``BIND(C)`` attribute has been added
to inform the compiler that a symbol shall be interoperable with C;
also, some constraints are added.  Note, however, that not
all C features have a Fortran equivalent or vice versa.  For instance,
neither C's unsigned integers nor C's functions with variable number
of arguments have an equivalent in Fortran.

Note that array dimensions are reversely ordered in C and that arrays in
C always start with index 0 while in Fortran they start by default with
1.  Thus, an array declaration ``A(n,m)`` in Fortran matches
``A[m][n]`` in C and accessing the element ``A(i,j)`` matches
``A[j-1][i-1]``.  The element following ``A(i,j)`` (C: ``A[j-1][i-1]`` ;
assuming i < n) in memory is ``A(i+1,j)`` (C: ``A[j-1][i]``).

.. index:: C intrinsic type interoperability, intrinsic type interoperability with C, interoperability, intrinsic type

.. _intrinsic-types:

Intrinsic Types
^^^^^^^^^^^^^^^

In order to ensure that exactly the same variable type and kind is used
in C and Fortran, you should use the named constants for kind parameters
that are defined in the ``ISO_C_BINDING`` intrinsic module.
That module contains named constants of character type representing
the escaped special characters in C, such as newline.
For a list of the constants, see :ref:`ISO_C_BINDING`.

For logical types, please note that the Fortran standard only guarantees
interoperability between C99's ``_Bool`` and Fortran's ``C_Bool`` -kind
logicals and C99 defines that ``true`` has the value 1 and ``false``
the value 0.  Using any other integer value with GNU Fortran's ``LOGICAL``
(with any kind parameter) gives an undefined result.  (Passing other integer
values than 0 and 1 to GCC's ``_Bool`` is also undefined, unless the
integer is explicitly or implicitly casted to ``_Bool``.)

.. index:: C derived type and struct interoperability, derived type interoperability with C, interoperability, derived type and struct

.. _derived-types-and-struct:

Derived Types and struct
^^^^^^^^^^^^^^^^^^^^^^^^

For compatibility of derived types with ``struct``, use
the ``BIND(C)`` attribute in the type declaration.  For instance, the
following type declaration

.. code-block:: fortran

   USE ISO_C_BINDING
   TYPE, BIND(C) :: myType
     INTEGER(C_INT) :: i1, i2
     INTEGER(C_SIGNED_CHAR) :: i3
     REAL(C_DOUBLE) :: d1
     COMPLEX(C_FLOAT_COMPLEX) :: c1
     CHARACTER(KIND=C_CHAR) :: str(5)
   END TYPE

matches the following ``struct`` declaration in C

.. code-block:: c

   struct {
     int i1, i2;
     /* Note: "char" might be signed or unsigned.  */
     signed char i3;
     double d1;
     float _Complex c1;
     char str[5];
   } myType;

Derived types with the C binding attribute shall not have the ``sequence``
attribute, type parameters, the ``extends`` attribute, nor type-bound
procedures.  Every component must be of interoperable type and kind and may not
have the ``pointer`` or ``allocatable`` attribute.  The names of the
components are irrelevant for interoperability.

As there exist no direct Fortran equivalents, neither unions nor structs
with bit field or variable-length array members are interoperable.

.. index:: C variable interoperability, variable interoperability with C, interoperability, variable

.. _interoperable-global-variables:

Interoperable Global Variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Variables can be made accessible from C using the C binding attribute,
optionally together with specifying a binding name.  Those variables
have to be declared in the declaration part of a ``MODULE``,
be of interoperable type, and have neither the ``pointer`` nor
the ``allocatable`` attribute.

.. code-block:: fortran

    MODULE m
      USE myType_module
      USE ISO_C_BINDING
      integer(C_INT), bind(C, name="_MyProject_flags") :: global_flag
      type(myType), bind(C) :: tp
    END MODULE

Here, ``_MyProject_flags`` is the case-sensitive name of the variable
as seen from C programs while ``global_flag`` is the case-insensitive
name as seen from Fortran.  If no binding name is specified, as for
:samp:`{tp}`, the C binding name is the (lowercase) Fortran binding name.
If a binding name is specified, only a single variable may be after the
double colon.  Note of warning: You cannot use a global variable to
access :samp:`{errno}` of the C library as the C standard allows it to be
a macro.  Use the ``IERRNO`` intrinsic (GNU extension) instead.

.. index:: C procedure interoperability, procedure interoperability with C, function interoperability with C, subroutine interoperability with C, interoperability, subroutine and function

.. _interoperable-subroutines-and-functions:

Interoperable Subroutines and Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Subroutines and functions have to have the ``BIND(C)`` attribute to
be compatible with C.  The dummy argument declaration is relatively
straightforward.  However, one needs to be careful because C uses
call-by-value by default while Fortran behaves usually similar to
call-by-reference.  Furthermore, strings and pointers are handled
differently.

To pass a variable by value, use the ``VALUE`` attribute.
Thus, the following C prototype

.. code-block:: fortran

  int func(int i, int *j)

matches the Fortran declaration

.. code-block:: fortran

    integer(c_int) function func(i,j)
      use iso_c_binding, only: c_int
      integer(c_int), VALUE :: i
      integer(c_int) :: j

Note that pointer arguments also frequently need the ``VALUE`` attribute,
see :ref:`working-with-c-pointers`.

Strings are handled quite differently in C and Fortran.  In C a string
is a ``NUL`` -terminated array of characters while in Fortran each string
has a length associated with it and is thus not terminated (by e.g.
``NUL``).  For example, if you want to use the following C function,

.. code-block:: c

    #include <stdio.h>
    void print_C(char *string) /* equivalent: char string[]  */
    {
       printf("%s\n", string);
    }

to print 'Hello World' from Fortran, you can call it using

.. code-block:: fortran

    use iso_c_binding, only: C_CHAR, C_NULL_CHAR
    interface
      subroutine print_c(string) bind(C, name="print_C")
        use iso_c_binding, only: c_char
        character(kind=c_char) :: string(*)
      end subroutine print_c
    end interface
    call print_c(C_CHAR_"Hello World"//C_NULL_CHAR)

As the example shows, you need to ensure that the
string is ``NUL`` terminated.  Additionally, the dummy argument
:samp:`{string}` of ``print_C`` is a length-one assumed-size
array; using ``character(len=*)`` is not allowed.  The example
above uses ``c_char_"Hello World"`` to ensure the string
literal has the right type; typically the default character
kind and ``c_char`` are the same and thus ``"Hello World"``
is equivalent.  However, the standard does not guarantee this.

The use of strings is now further illustrated using the C library
function ``strncpy``, whose prototype is

.. code-block:: c

    char *strncpy(char *restrict s1, const char *restrict s2, size_t n);

The function ``strncpy`` copies at most :samp:`{n}` characters from
string :samp:`{s2}` to :samp:`{s1}` and returns :samp:`{s1}`.  In the following
example, we ignore the return value:

.. code-block:: fortran

    use iso_c_binding
    implicit none
    character(len=30) :: str,str2
    interface
      ! Ignore the return value of strncpy -> subroutine
      ! "restrict" is always assumed if we do not pass a pointer
      subroutine strncpy(dest, src, n) bind(C)
        import
        character(kind=c_char),  intent(out) :: dest(*)
        character(kind=c_char),  intent(in)  :: src(*)
        integer(c_size_t), value, intent(in) :: n
      end subroutine strncpy
    end interface
    str = repeat('X',30) ! Initialize whole string with 'X'
    call strncpy(str, c_char_"Hello World"//C_NULL_CHAR, &
                 len(c_char_"Hello World",kind=c_size_t))
    print '(a)', str ! prints: "Hello WorldXXXXXXXXXXXXXXXXXXX"
    end

The intrinsic procedures are described in :ref:`intrinsic-procedures`.

.. index:: C pointers, pointers, C

.. _working-with-c-pointers:

Working with C Pointers
^^^^^^^^^^^^^^^^^^^^^^^

C pointers are represented in Fortran via the special opaque derived
type ``type(c_ptr)`` (with private components).  C pointers are distinct
from Fortran objects with the ``POINTER`` attribute.  Thus one needs to
use intrinsic conversion procedures to convert from or to C pointers.
For some applications, using an assumed type (``TYPE(*)``) can be
an alternative to a C pointer, and you can also use library routines
to access Fortran pointers from C.  See :ref:`further-interoperability-of-fortran-with-c`.

Here is an example of using C pointers in Fortran:

.. code-block:: fortran

    use iso_c_binding
    type(c_ptr) :: cptr1, cptr2
    integer, target :: array(7), scalar
    integer, pointer :: pa(:), ps
    cptr1 = c_loc(array(1)) ! The programmer needs to ensure that the
                            ! array is contiguous if required by the C
                            ! procedure
    cptr2 = c_loc(scalar)
    call c_f_pointer(cptr2, ps)
    call c_f_pointer(cptr2, pa, shape=[7])

When converting C to Fortran arrays, the one-dimensional ``SHAPE`` argument
has to be passed.

If a pointer is a dummy argument of an interoperable procedure, it usually
has to be declared using the ``VALUE`` attribute.  ``void*``
matches ``TYPE(C_PTR), VALUE``, while ``TYPE(C_PTR)`` alone
matches ``void**``.

Procedure pointers are handled analogously to pointers; the C type is
``TYPE(C_FUNPTR)`` and the intrinsic conversion procedures are
``C_F_PROCPOINTER`` and ``C_FUNLOC``.

Let us consider two examples of actually passing a procedure pointer from
C to Fortran and vice versa.  Note that these examples are also very
similar to passing ordinary pointers between both languages. First,
consider this code in C:

.. code-block:: c

  /* Procedure implemented in Fortran.  */
  void get_values (void (*)(double));

  /* Call-back routine we want called from Fortran.  */
  void
  print_it (double x)
  {
    printf ("Number is %f.\n", x);
  }

  /* Call Fortran routine and pass call-back to it.  */
  void
  foobar ()
  {
    get_values (&print_it);
  }

A matching implementation for ``get_values`` in Fortran, that correctly
receives the procedure pointer from C and is able to call it, is given
in the following ``MODULE`` :

.. code-block:: fortran

  MODULE m
    IMPLICIT NONE

    ! Define interface of call-back routine.
    ABSTRACT INTERFACE
      SUBROUTINE callback (x)
        USE, INTRINSIC :: ISO_C_BINDING
        REAL(KIND=C_DOUBLE), INTENT(IN), VALUE :: x
      END SUBROUTINE callback
    END INTERFACE

  CONTAINS

    ! Define C-bound procedure.
    SUBROUTINE get_values (cproc) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING
      TYPE(C_FUNPTR), INTENT(IN), VALUE :: cproc

      PROCEDURE(callback), POINTER :: proc

      ! Convert C to Fortran procedure pointer.
      CALL C_F_PROCPOINTER (cproc, proc)

      ! Call it.
      CALL proc (1.0_C_DOUBLE)
      CALL proc (-42.0_C_DOUBLE)
      CALL proc (18.12_C_DOUBLE)
    END SUBROUTINE get_values

  END MODULE m

Next, we want to call a C routine that expects a procedure pointer argument
and pass it a Fortran procedure (which clearly must be interoperable!).
Again, the C function may be:

.. code-block:: c

  int
  call_it (int (*func)(int), int arg)
  {
    return func (arg);
  }

It can be used as in the following Fortran code:

.. code-block:: fortran

  MODULE m
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE

    ! Define interface of C function.
    INTERFACE
      INTEGER(KIND=C_INT) FUNCTION call_it (func, arg) BIND(C)
        USE, INTRINSIC :: ISO_C_BINDING
        TYPE(C_FUNPTR), INTENT(IN), VALUE :: func
        INTEGER(KIND=C_INT), INTENT(IN), VALUE :: arg
      END FUNCTION call_it
    END INTERFACE

  CONTAINS

    ! Define procedure passed to C function.
    ! It must be interoperable!
    INTEGER(KIND=C_INT) FUNCTION double_it (arg) BIND(C)
      INTEGER(KIND=C_INT), INTENT(IN), VALUE :: arg
      double_it = arg + arg
    END FUNCTION double_it

    ! Call C function.
    SUBROUTINE foobar ()
      TYPE(C_FUNPTR) :: cproc
      INTEGER(KIND=C_INT) :: i

      ! Get C procedure pointer.
      cproc = C_FUNLOC (double_it)

      ! Use it.
      DO i = 1_C_INT, 10_C_INT
        PRINT *, call_it (cproc, i)
      END DO
    END SUBROUTINE foobar

  END MODULE m

.. index:: Further Interoperability of Fortran with C, TS 29113, array descriptor, dope vector, assumed-type, assumed-rank

.. _further-interoperability-of-fortran-with-c:

Further Interoperability of Fortran with C
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GNU Fortran implements the Technical Specification ISO/IEC TS
29113:2012, which extends the interoperability support of Fortran 2003
and Fortran 2008 and is now part of the 2018 Fortran standard.
Besides removing some restrictions and constraints, the Technical
Specification adds assumed-type (``TYPE(*)``) and assumed-rank
(``DIMENSION(..)``) variables and allows for interoperability of
assumed-shape, assumed-rank, and deferred-shape arrays, as well as
allocatables and pointers.  Objects of these types are passed to
``BIND(C)`` functions as descriptors with a standard interface,
declared in the header file ``<ISO_Fortran_binding.h>``.

Note: Currently, GNU Fortran does not use internally the array descriptor
(dope vector) as specified in the Technical Specification, but uses
an array descriptor with different fields in functions without the
``BIND(C)`` attribute.  Arguments to functions marked ``BIND(C)``
are converted to the specified form.  If you need to access GNU Fortran's
internal array descriptor, you can use the Chasm Language Interoperability
Tools, http://chasm-interop.sourceforge.net/.
