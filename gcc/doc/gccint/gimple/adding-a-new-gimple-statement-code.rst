..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Adding a new GIMPLE statement code

.. _adding-a-new-gimple-statement-code:

Adding a new GIMPLE statement code
**********************************

The first step in adding a new GIMPLE statement code, is
modifying the file ``gimple.def``, which contains all the GIMPLE
codes.  Then you must add a corresponding gimple subclass
located in ``gimple.h``.  This in turn, will require you to add a
corresponding ``GTY`` tag in ``gsstruct.def``, and code to handle
this tag in ``gss_for_code`` which is located in ``gimple.cc``.

In order for the garbage collector to know the size of the
structure you created in ``gimple.h``, you need to add a case to
handle your new GIMPLE statement in ``gimple_size`` which is located
in ``gimple.cc``.

You will probably want to create a function to build the new
gimple statement in ``gimple.cc``.  The function should be called
``gimple_build_new-tuple-name``, and should return the new tuple
as a pointer to the appropriate gimple subclass.

If your new statement requires accessors for any members or
operands it may have, put simple inline accessors in
``gimple.h`` and any non-trivial accessors in ``gimple.cc`` with a
corresponding prototype in ``gimple.h``.

You should add the new statement subclass to the class hierarchy diagram
in ``gimple.texi``.