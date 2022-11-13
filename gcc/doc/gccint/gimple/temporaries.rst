..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Temporaries

.. _temporaries:

Temporaries
***********

When gimplification encounters a subexpression that is too
complex, it creates a new temporary variable to hold the value of
the subexpression, and adds a new statement to initialize it
before the current statement. These special temporaries are known
as :samp:`expression temporaries`, and are allocated using
``get_formal_tmp_var``.  The compiler tries to always evaluate
identical expressions into the same temporary, to simplify
elimination of redundant calculations.

We can only use expression temporaries when we know that it will
not be reevaluated before its value is used, and that it will not
be otherwise modified [#f1]_.

. Other temporaries can be allocated
using ``get_initialized_tmp_var`` or ``create_tmp_var``.

Currently, an expression like ``a = b + 5`` is not reduced any
further.  We tried converting it to something like

.. code-block:: c++

  T1 = b + 5;
  a = T1;

but this bloated the representation for minimal benefit.  However, a
variable which must live in memory cannot appear in an expression; its
value is explicitly loaded into a temporary first.  Similarly, storing
the value of an expression to a memory variable goes through a
temporary.

.. [#f1] These restrictions are derived from those in Morgan 4.8.