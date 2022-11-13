..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gnu-fortran-compiler-directives:

GNU Fortran Compiler Directives
*******************************

.. toctree::
  :maxdepth: 2


.. _attributes-directive:

ATTRIBUTES directive
^^^^^^^^^^^^^^^^^^^^

The Fortran standard describes how a conforming program shall
behave; however, the exact implementation is not standardized.  In order
to allow the user to choose specific implementation details, compiler
directives can be used to set attributes of variables and procedures
which are not part of the standard.  Whether a given attribute is
supported and its exact effects depend on both the operating system and
on the processor; see
:ref:`gcc:top`
for details.

For procedures and procedure pointers, the following attributes can
be used to change the calling convention:

* ``CDECL`` -- standard C calling convention

* ``STDCALL`` -- convention where the called procedure pops the stack

* ``FASTCALL`` -- part of the arguments are passed via registers
  instead using the stack

Besides changing the calling convention, the attributes also influence
the decoration of the symbol name, e.g., by a leading underscore or by
a trailing at-sign followed by the number of bytes on the stack.  When
assigning a procedure to a procedure pointer, both should use the same
calling convention.

On some systems, procedures and global variables (module variables and
``COMMON`` blocks) need special handling to be accessible when they
are in a shared library.  The following attributes are available:

* ``DLLEXPORT`` -- provide a global pointer to a pointer in the DLL

* ``DLLIMPORT`` -- reference the function or variable using a
  global pointer

For dummy arguments, the ``NO_ARG_CHECK`` attribute can be used; in
other compilers, it is also known as ``IGNORE_TKR``.  For dummy arguments
with this attribute actual arguments of any type and kind (similar to
``TYPE(*)``), scalars and arrays of any rank (no equivalent
in Fortran standard) are accepted.  As with ``TYPE(*)``, the argument
is unlimited polymorphic and no type information is available.
Additionally, the argument may only be passed to dummy arguments
with the ``NO_ARG_CHECK`` attribute and as argument to the
``PRESENT`` intrinsic function and to ``C_LOC`` of the
``ISO_C_BINDING`` module.

Variables with ``NO_ARG_CHECK`` attribute shall be of assumed-type
(``TYPE(*)`` ; recommended) or of type ``INTEGER``, ``LOGICAL``,
``REAL`` or ``COMPLEX``. They shall not have the ``ALLOCATE``,
``CODIMENSION``, ``INTENT(OUT)``, ``POINTER`` or ``VALUE``
attribute; furthermore, they shall be either scalar or of assumed-size
(``dimension(*)``). As ``TYPE(*)``, the ``NO_ARG_CHECK`` attribute
requires an explicit interface.

* ``NO_ARG_CHECK`` -- disable the type, kind and rank checking

* ``DEPRECATED`` -- print a warning when using a such-tagged
  deprecated procedure, variable or parameter; the warning can be suppressed
  with :option:`-Wno-deprecated-declarations`.

The attributes are specified using the syntax

``!GCC$ ATTRIBUTES`` :samp:`{attribute-list}` ``::`` :samp:`{variable-list}`

where in free-form source code only whitespace is allowed before ``!GCC$``
and in fixed-form source code ``!GCC$``, ``cGCC$`` or ``*GCC$`` shall
start in the first column.

For procedures, the compiler directives shall be placed into the body
of the procedure; for variables and procedure pointers, they shall be in
the same declaration part as the variable or procedure pointer.

.. _unroll-directive:

UNROLL directive
^^^^^^^^^^^^^^^^

The syntax of the directive is

``!GCC$ unroll N``

You can use this directive to control how many times a loop should be unrolled.
It must be placed immediately before a ``DO`` loop and applies only to the
loop that follows.  N is an integer constant specifying the unrolling factor.
The values of 0 and 1 block any unrolling of the loop.

.. _builtin-directive:

BUILTIN directive
^^^^^^^^^^^^^^^^^

The syntax of the directive is

``!GCC$ BUILTIN (B) attributes simd FLAGS IF('target')``

You can use this directive to define which middle-end built-ins provide vector
implementations.  ``B`` is name of the middle-end built-in.  ``FLAGS``
are optional and must be either "(inbranch)" or "(notinbranch)".
``IF`` statement is optional and is used to filter multilib ABIs
for the built-in that should be vectorized.  Example usage:

.. code-block:: fortran

  !GCC$ builtin (sinf) attributes simd (notinbranch) if('x86_64')

The purpose of the directive is to provide an API among the GCC compiler and
the GNU C Library which would define vector implementations of math routines.

.. _ivdep-directive:

IVDEP directive
^^^^^^^^^^^^^^^

The syntax of the directive is

``!GCC$ ivdep``

This directive tells the compiler to ignore vector dependencies in the
following loop.  It must be placed immediately before a ``DO`` loop
and applies only to the loop that follows.

Sometimes the compiler may not have sufficient information to decide
whether a particular loop is vectorizable due to potential
dependencies between iterations.  The purpose of the directive is to
tell the compiler that vectorization is safe.

This directive is intended for annotation of existing code.  For new
code it is recommended to consider OpenMP SIMD directives as potential
alternative.

.. _vector-directive:

VECTOR directive
^^^^^^^^^^^^^^^^

The syntax of the directive is

``!GCC$ vector``

This directive tells the compiler to vectorize the following loop.  It
must be placed immediately before a ``DO`` loop and applies only to
the loop that follows.

.. _novector-directive:

NOVECTOR directive
^^^^^^^^^^^^^^^^^^

The syntax of the directive is

``!GCC$ novector``

This directive tells the compiler to not vectorize the following loop.
It must be placed immediately before a ``DO`` loop and applies only
to the loop that follows.