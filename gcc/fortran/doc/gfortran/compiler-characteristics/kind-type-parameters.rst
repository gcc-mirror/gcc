..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: kind

.. _kind-type-parameters:

KIND Type Parameters
********************

The ``KIND`` type parameters supported by GNU Fortran for the primitive
data types are:

``INTEGER``

  1, 2, 4, 8 [#f1]_, 16 [#f1]_, default: 4 [#f2]_

``LOGICAL``

  1, 2, 4, 8 [#f1]_, 16 [#f1]_, default: 4 [#f2]_

``REAL``

  4, 8, 10 [#f1]_, 16 [#f1]_, default: 4 [#f3]_

``COMPLEX``

  4, 8, 10 [#f1]_, 16 [#f1]_, default: 4 [#f3]_

``DOUBLE PRECISION``

  4, 8, 10 [#f1]_, 16 [#f1]_, default: 8 [#f3]_

``CHARACTER``

  1, 4, default: 1

.. [#f1] not available on all systems
.. [#f2] unless :option:`-fdefault-integer-8` is used
.. [#f3] unless :option:`-fdefault-real-8` is used (see :ref:`fortran-dialect-options`)

The ``KIND`` value matches the storage size in bytes, except for
``COMPLEX`` where the storage size is twice as much (or both real and
imaginary part are a real value of the given size).  It is recommended to use
the :ref:`SELECTED_CHAR_KIND`, :ref:`SELECTED_INT_KIND` and
:ref:`SELECTED_REAL_KIND` intrinsics or the ``INT8``, ``INT16``,
``INT32``, ``INT64``, ``REAL32``, ``REAL64``, and ``REAL128``
parameters of the ``ISO_FORTRAN_ENV`` module instead of the concrete values.
The available kind parameters can be found in the constant arrays
``CHARACTER_KINDS``, ``INTEGER_KINDS``, ``LOGICAL_KINDS`` and
``REAL_KINDS`` in the :ref:`ISO_FORTRAN_ENV` module.  For C interoperability,
the kind parameters of the :ref:`ISO_C_BINDING` module should be used.
