..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _type-bound-procedures:

Type-bound Procedures
*********************

Type-bound procedures are stored in the ``tb_sym_root`` of the namespace
``f2k_derived`` associated with the derived-type symbol as ``gfc_symtree``
nodes.  The name and symbol of these symtrees corresponds to the binding-name
of the procedure, i.e. the name that is used to call it from the context of an
object of the derived-type.

In addition, this type of symtrees stores in ``n.tb`` a struct of type
``gfc_typebound_proc`` containing the additional data needed:  The
binding attributes (like ``PASS`` and ``NOPASS``, ``NON_OVERRIDABLE``
or the access-specifier), the binding's target(s) and, if the current binding
overrides or extends an inherited binding of the same name, ``overridden``
points to this binding's ``gfc_typebound_proc`` structure.

Specific Bindings
^^^^^^^^^^^^^^^^^

.. -

For specific bindings (declared with ``PROCEDURE``), if they have a
passed-object argument, the passed-object dummy argument is first saved by its
name, and later during resolution phase the corresponding argument is looked for
and its position remembered as ``pass_arg_num`` in ``gfc_typebound_proc``.
The binding's target procedure is pointed-to by ``u.specific``.

``DEFERRED`` bindings are just like ordinary specific bindings, except
that their ``deferred`` flag is set of course and that ``u.specific``
points to their 'interface' defining symbol (might be an abstract interface)
instead of the target procedure.

At the moment, all type-bound procedure calls are statically dispatched and
transformed into ordinary procedure calls at resolution time; their actual
argument list is updated to include at the right position the passed-object
argument, if applicable, and then a simple procedure call to the binding's
target procedure is built.  To handle dynamic dispatch in the future, this will
be extended to allow special code generation during the trans-phase to dispatch
based on the object's dynamic type.

Generic Bindings
^^^^^^^^^^^^^^^^

.. -

Bindings declared as ``GENERIC`` store the specific bindings they target as
a linked list using nodes of type ``gfc_tbp_generic`` in ``u.generic``.
For each specific target, the parser records its symtree and during resolution
this symtree is bound to the corresponding ``gfc_typebound_proc`` structure
of the specific target.

Calls to generic bindings are handled entirely in the resolution-phase, where
for the actual argument list present the matching specific binding is found
and the call's target procedure (``value.compcall.tbp``) is re-pointed to
the found specific binding and this call is subsequently handled by the logic
for specific binding calls.

Calls to Type-bound Procedures
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. -

Calls to type-bound procedures are stored in the parse-tree as ``gfc_expr``
nodes of type ``EXPR_COMPCALL``.  Their ``value.compcall.actual`` saves
the actual argument list of the call and ``value.compcall.tbp`` points to the
``gfc_typebound_proc`` structure of the binding to be called.  The object
in whose context the procedure was called is saved by combination of
``symtree`` and ``ref``, as if the expression was of type
``EXPR_VARIABLE``.

For code like this:

.. code-block:: fortran

  CALL myobj%procedure (arg1, arg2)

the ``CALL`` is represented in the parse-tree as a ``gfc_code`` node of
type ``EXEC_COMPCALL``.  The ``expr`` member of this node holds an
expression of type ``EXPR_COMPCALL`` of the same structure as mentioned above
except that its target procedure is of course a ``SUBROUTINE`` and not a
``FUNCTION``.

Expressions that are generated internally (as expansion of a type-bound
operator call) may also use additional flags and members.
``value.compcall.ignore_pass`` signals that even though a ``PASS``
attribute may be present the actual argument list should not be updated because
it already contains the passed-object.
``value.compcall.base_object`` overrides, if it is set, the base-object
(that is normally stored in ``symtree`` and ``ref`` as mentioned above);
this is needed because type-bound operators can be called on a base-object that
need not be of type ``EXPR_VARIABLE`` and thus representable in this way.
Finally, if ``value.compcall.assign`` is set, the call was produced in
expansion of a type-bound assignment; this means that proper dependency-checking
needs to be done when relevant.
