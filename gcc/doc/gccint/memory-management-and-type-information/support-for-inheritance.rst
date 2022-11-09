..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _inheritance-and-gty:

Support for inheritance
***********************

gengtype has some support for simple class hierarchies.  You can use
this to have gengtype autogenerate marking routines, provided:

* There must be a concrete base class, with a discriminator expression
  that can be used to identify which subclass an instance is.

* Only single inheritance is used.

* None of the classes within the hierarchy are templates.

If your class hierarchy does not fit in this pattern, you must use
:ref:`user-gc` instead.

The base class and its discriminator must be identified using the 'desc'
option.  Each concrete subclass must use the 'tag' option to identify
which value of the discriminator it corresponds to.

Every class in the hierarchy must have a ``GTY(())`` marker, as
gengtype will only attempt to parse classes that have such a marker [#f1]_.

.. code-block:: c++

  class GTY((desc("%h.kind"), tag("0"))) example_base
  {
  public:
      int kind;
      tree a;
  };

  class GTY((tag("1"))) some_subclass : public example_base
  {
  public:
      tree b;
  };

  class GTY((tag("2"))) some_other_subclass : public example_base
  {
  public:
      tree c;
  };

The generated marking routines for the above will contain a 'switch'
on 'kind', visiting all appropriate fields.  For example, if kind is
2, it will cast to 'some_other_subclass' and visit fields a, b, and c.

.. [#f1] Classes lacking such a marker will not be identified as being
  part of the hierarchy, and so the marking routines will not handle them,
  leading to a assertion failure within the marking routines due to an
  unknown tag value (assuming that assertions are enabled).
