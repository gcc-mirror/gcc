..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _interoperability-options:

Options for interoperability with other languages
*************************************************

.. index:: c-prototypes, Generating C prototypes from Fortran BIND(C) enteties

.. option:: -fc-prototypes

  This option will generate C prototypes from ``BIND(C)`` variable
  declarations, types and procedure interfaces and writes them to
  standard output.  ``ENUM`` is not yet supported.

  The generated prototypes may need inclusion of an appropriate header,
  such as ``<stdint.h>`` or ``<stdlib.h>``.  For types which are
  not specified using the appropriate kind from the ``iso_c_binding``
  module, a warning is added as a comment to the code.

  For function pointers, a pointer to a function returning ``int``
  without an explicit argument list is generated.

  Example of use:

  .. code-block:: shell-session

    $ gfortran -fc-prototypes -fsyntax-only foo.f90 > foo.h

  where the C code intended for interoperating with the Fortran code
  then  uses ``#include "foo.h"``.

.. index:: c-prototypes-external, Generating C prototypes from external procedures

.. option:: -fc-prototypes-external

  This option will generate C prototypes from external functions and
  subroutines and write them to standard output.  This may be useful for
  making sure that C bindings to Fortran code are correct.  This option
  does not generate prototypes for ``BIND(C)`` procedures, use
  :option:`-fc-prototypes` for that.

  The generated prototypes may need inclusion of an appropriate
  header, such as ``<stdint.h>`` or ``<stdlib.h>``.

  This is primarily meant for legacy code to ensure that existing C
  bindings match what :command:`gfortran` emits.  The generated C
  prototypes should be correct for the current version of the compiler,
  but may not match what other compilers or earlier versions of
  :command:`gfortran` need.  For new developments, use of the
  ``BIND(C)`` features is recommended.

  Example of use:

  .. code-block:: shell-session

    $ gfortran -fc-prototypes-external -fsyntax-only foo.f > foo.h

  where the C code intended for interoperating with the Fortran code
  then  uses ``#include "foo.h"``.
