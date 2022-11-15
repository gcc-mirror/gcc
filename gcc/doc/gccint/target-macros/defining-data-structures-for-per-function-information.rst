..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: per-function data, data structures

.. _per-function-data:

Defining data structures for per-function information.
******************************************************

If the target needs to store information on a per-function basis, GCC
provides a macro and a couple of variables to allow this.  Note, just
using statics to store the information is a bad idea, since GCC supports
nested functions, so you can be halfway through encoding one function
when another one comes along.

GCC defines a data structure called ``struct function`` which
contains all of the data specific to an individual function.  This
structure contains a field called ``machine`` whose type is
``struct machine_function *``, which can be used by targets to point
to their own specific data.

If a target needs per-function specific data it should define the type
``struct machine_function`` and also the macro ``INIT_EXPANDERS``.
This macro should be used to initialize the function pointer
``init_machine_status``.  This pointer is explained below.

One typical use of per-function, target specific data is to create an
RTX to hold the register containing the function's return address.  This
RTX can then be used to implement the ``__builtin_return_address``
function, for level 0.

Note---earlier implementations of GCC used a single data area to hold
all of the per-function information.  Thus when processing of a nested
function began the old per-function data had to be pushed onto a
stack, and when the processing was finished, it had to be popped off the
stack.  GCC used to provide function pointers called
``save_machine_status`` and ``restore_machine_status`` to handle
the saving and restoring of the target specific information.  Since the
single data area approach is no longer used, these pointers are no
longer supported.

.. c:macro:: INIT_EXPANDERS

  Macro called to initialize any target specific information.  This macro
  is called once per function, before generation of any RTL has begun.
  The intention of this macro is to allow the initialization of the
  function pointer ``init_machine_status``.

.. index:: init_machine_status

Variable void (\*)(struct function \*) init_machine_statusIf this function pointer is non- ``NULL`` it will be called once per
function, before function compilation starts, in order to allow the
target to perform any target specific initialization of the
``struct function`` structure.  It is intended that this would be
used to initialize the ``machine`` of that structure.

``struct machine_function`` structures are expected to be freed by GC.
Generally, any memory that they reference must be allocated by using
GC allocation, including the structure itself.
