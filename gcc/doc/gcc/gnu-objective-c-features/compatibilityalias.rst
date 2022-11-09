..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _compatibility_alias:

compatibility_alias
*******************

The keyword ``@compatibility_alias`` allows you to define a class name
as equivalent to another class name.  For example:

.. code-block:: objective-c

  @compatibility_alias WOApplication GSWApplication;

tells the compiler that each time it encounters ``WOApplication`` as
a class name, it should replace it with ``GSWApplication`` (that is,
``WOApplication`` is just an alias for ``GSWApplication``).

There are some constraints on how this can be used---

* ``WOApplication`` (the alias) must not be an existing class;

* ``GSWApplication`` (the real class) must be an existing class.
