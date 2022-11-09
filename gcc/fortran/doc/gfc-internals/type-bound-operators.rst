..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _type-bound-operators:

Type-bound Operators
********************

Type-bound operators are in fact basically just ``GENERIC`` procedure
bindings and are represented much in the same way as those (see
:ref:`type-bound-procedures`).

They come in two flavours:
User-defined operators (like ``.MYOPERATOR.``)
are stored in the ``f2k_derived`` namespace's ``tb_uop_root``
symtree exactly like ordinary type-bound procedures are stored in
``tb_sym_root`` ; their symtrees' names are the operator-names (e.g.
:samp:`myoperator` in the example).
Intrinsic operators on the other hand are stored in the namespace's
array member ``tb_op`` indexed by the intrinsic operator's enum
value.  Those need not be packed into ``gfc_symtree`` structures and are
only ``gfc_typebound_proc`` instances.

When an operator call or assignment is found that cannot be handled in
another way (i.e. neither matches an intrinsic nor interface operator
definition) but that contains a derived-type expression, all type-bound
operators defined on that derived-type are checked for a match with
the operator call.  If there's indeed a relevant definition, the
operator call is replaced with an internally generated ``GENERIC``
type-bound procedure call to the respective definition and that call is
further processed.
