..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: operands, virtual operands, real operands, update_stmt

.. _ssa-operands:

SSA Operands
************

Almost every GIMPLE statement will contain a reference to a variable
or memory location.  Since statements come in different shapes and
sizes, their operands are going to be located at various spots inside
the statement's tree.  To facilitate access to the statement's
operands, they are organized into lists associated inside each
statement's annotation.  Each element in an operand list is a pointer
to a ``VAR_DECL``, ``PARM_DECL`` or ``SSA_NAME`` tree node.
This provides a very convenient way of examining and replacing
operands.

Data flow analysis and optimization is done on all tree nodes
representing variables.  Any node for which ``SSA_VAR_P`` returns
nonzero is considered when scanning statement operands.  However, not
all ``SSA_VAR_P`` variables are processed in the same way.  For the
purposes of optimization, we need to distinguish between references to
local scalar variables and references to globals, statics, structures,
arrays, aliased variables, etc.  The reason is simple, the compiler
can gather complete data flow information for a local scalar.  On the
other hand, a global variable may be modified by a function call, it
may not be possible to keep track of all the elements of an array or
the fields of a structure, etc.

The operand scanner gathers two kinds of operands: :dfn:`real` and
:dfn:`virtual`.  An operand for which ``is_gimple_reg`` returns true
is considered real, otherwise it is a virtual operand.  We also
distinguish between uses and definitions.  An operand is used if its
value is loaded by the statement (e.g., the operand at the RHS of an
assignment).  If the statement assigns a new value to the operand, the
operand is considered a definition (e.g., the operand at the LHS of
an assignment).

Virtual and real operands also have very different data flow
properties.  Real operands are unambiguous references to the
full object that they represent.  For instance, given

.. code-block:: c++

  {
    int a, b;
    a = b
  }

Since ``a`` and ``b`` are non-aliased locals, the statement
``a = b`` will have one real definition and one real use because
variable ``a`` is completely modified with the contents of
variable ``b``.  Real definition are also known as :dfn:`killing
definitions`.  Similarly, the use of ``b`` reads all its bits.

In contrast, virtual operands are used with variables that can have
a partial or ambiguous reference.  This includes structures, arrays,
globals, and aliased variables.  In these cases, we have two types of
definitions.  For globals, structures, and arrays, we can determine from
a statement whether a variable of these types has a killing definition.
If the variable does, then the statement is marked as having a
:dfn:`must definition` of that variable.  However, if a statement is only
defining a part of the variable (i.e. a field in a structure), or if we
know that a statement might define the variable but we cannot say for sure,
then we mark that statement as having a :dfn:`may definition`.  For
instance, given

.. code-block:: c++

  {
    int a, b, *p;

    if (...)
      p = &a;
    else
      p = &b;
    *p = 5;
    return *p;
  }

The assignment ``*p = 5`` may be a definition of ``a`` or
``b``.  If we cannot determine statically where ``p`` is
pointing to at the time of the store operation, we create virtual
definitions to mark that statement as a potential definition site for
``a`` and ``b``.  Memory loads are similarly marked with virtual
use operands.  Virtual operands are shown in tree dumps right before
the statement that contains them.  To request a tree dump with virtual
operands, use the :option:`-vops` option to :option:`-fdump-tree` :

.. code-block:: c++

  {
    int a, b, *p;

    if (...)
      p = &a;
    else
      p = &b;
    # a = VDEF <a>
    # b = VDEF <b>
    *p = 5;

    # VUSE <a>
    # VUSE <b>
    return *p;
  }

Notice that ``VDEF`` operands have two copies of the referenced
variable.  This indicates that this is not a killing definition of
that variable.  In this case we refer to it as a :dfn:`may definition`
or :dfn:`aliased store`.  The presence of the second copy of the
variable in the ``VDEF`` operand will become important when the
function is converted into SSA form.  This will be used to link all
the non-killing definitions to prevent optimizations from making
incorrect assumptions about them.

Operands are updated as soon as the statement is finished via a call
to ``update_stmt``.  If statement elements are changed via
``SET_USE`` or ``SET_DEF``, then no further action is required
(i.e., those macros take care of updating the statement).  If changes
are made by manipulating the statement's tree directly, then a call
must be made to ``update_stmt`` when complete.  Calling one of the
``bsi_insert`` routines or ``bsi_replace`` performs an implicit
call to ``update_stmt``.

.. index:: Operand Iterators, Operand Access Routines

Operand Iterators And Access Routines
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Operands are collected by :samp:`tree-ssa-operands.cc`.  They are stored
inside each statement's annotation and can be accessed through either the
operand iterators or an access routine.

The following access routines are available for examining operands:

* ``SINGLE_SSA_{USE,DEF,TREE}_OPERAND`` : These accessors will return
  NULL unless there is exactly one operand matching the specified flags.  If
  there is exactly one operand, the operand is returned as either a ``tree``,
  ``def_operand_p``, or ``use_operand_p``.

  .. code-block:: c++

    tree t = SINGLE_SSA_TREE_OPERAND (stmt, flags);
    use_operand_p u = SINGLE_SSA_USE_OPERAND (stmt, SSA_ALL_VIRTUAL_USES);
    def_operand_p d = SINGLE_SSA_DEF_OPERAND (stmt, SSA_OP_ALL_DEFS);

* ``ZERO_SSA_OPERANDS`` : This macro returns true if there are no
  operands matching the specified flags.

  .. code-block:: c++

    if (ZERO_SSA_OPERANDS (stmt, SSA_OP_ALL_VIRTUALS))
      return;

* ``NUM_SSA_OPERANDS`` : This macro Returns the number of operands
  matching 'flags'.  This actually executes a loop to perform the count, so
  only use this if it is really needed.

  .. code-block:: c++

    int count = NUM_SSA_OPERANDS (stmt, flags)

If you wish to iterate over some or all operands, use the
``FOR_EACH_SSA_{USE,DEF,TREE}_OPERAND`` iterator.  For example, to print
all the operands for a statement:

.. code-block:: c++

  void
  print_ops (tree stmt)
  {
    ssa_op_iter;
    tree var;

    FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_ALL_OPERANDS)
      print_generic_expr (stderr, var, TDF_SLIM);
  }

How to choose the appropriate iterator:

* Determine whether you are need to see the operand pointers, or just the
  trees, and choose the appropriate macro:

  .. code-block:: c++

    Need            Macro:
    ----            -------
    use_operand_p   FOR_EACH_SSA_USE_OPERAND
    def_operand_p   FOR_EACH_SSA_DEF_OPERAND
    tree            FOR_EACH_SSA_TREE_OPERAND

* You need to declare a variable of the type you are interested
  in, and an ssa_op_iter structure which serves as the loop controlling
  variable.

* Determine which operands you wish to use, and specify the flags of
  those you are interested in.  They are documented in
  :samp:`tree-ssa-operands.h`:

  .. code-block:: c++

    #define SSA_OP_USE              0x01    /* Real USE operands.  */
    #define SSA_OP_DEF              0x02    /* Real DEF operands.  */
    #define SSA_OP_VUSE             0x04    /* VUSE operands.  */
    #define SSA_OP_VDEF             0x08    /* VDEF operands.  */

    /* These are commonly grouped operand flags.  */
    #define SSA_OP_VIRTUAL_USES	(SSA_OP_VUSE)
    #define SSA_OP_VIRTUAL_DEFS	(SSA_OP_VDEF)
    #define SSA_OP_ALL_VIRTUALS     (SSA_OP_VIRTUAL_USES | SSA_OP_VIRTUAL_DEFS)
    #define SSA_OP_ALL_USES		(SSA_OP_VIRTUAL_USES | SSA_OP_USE)
    #define SSA_OP_ALL_DEFS		(SSA_OP_VIRTUAL_DEFS | SSA_OP_DEF)
    #define SSA_OP_ALL_OPERANDS	(SSA_OP_ALL_USES | SSA_OP_ALL_DEFS)

So if you want to look at the use pointers for all the ``USE`` and
``VUSE`` operands, you would do something like:

.. code-block:: c++

    use_operand_p use_p;
    ssa_op_iter iter;

    FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, (SSA_OP_USE | SSA_OP_VUSE))
      {
        process_use_ptr (use_p);
      }

The ``TREE`` macro is basically the same as the ``USE`` and
``DEF`` macros, only with the use or def dereferenced via
``USE_FROM_PTR (use_p)`` and ``DEF_FROM_PTR (def_p)``.  Since we
aren't using operand pointers, use and defs flags can be mixed.

.. code-block:: c++

    tree var;
    ssa_op_iter iter;

    FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_VUSE)
      {
         print_generic_expr (stderr, var, TDF_SLIM);
      }

``VDEF`` s are broken into two flags, one for the
``DEF`` portion (``SSA_OP_VDEF``) and one for the USE portion
(``SSA_OP_VUSE``).

There are many examples in the code, in addition to the documentation
in :samp:`tree-ssa-operands.h` and :samp:`ssa-iterators.h`.

There are also a couple of variants on the stmt iterators regarding PHI
nodes.

``FOR_EACH_PHI_ARG`` Works exactly like
``FOR_EACH_SSA_USE_OPERAND``, except it works over ``PHI`` arguments
instead of statement operands.

.. code-block:: c++

  /* Look at every virtual PHI use.  */
  FOR_EACH_PHI_ARG (use_p, phi_stmt, iter, SSA_OP_VIRTUAL_USES)
  {
     my_code;
  }

  /* Look at every real PHI use.  */
  FOR_EACH_PHI_ARG (use_p, phi_stmt, iter, SSA_OP_USES)
    my_code;

  /* Look at every PHI use.  */
  FOR_EACH_PHI_ARG (use_p, phi_stmt, iter, SSA_OP_ALL_USES)
    my_code;

``FOR_EACH_PHI_OR_STMT_{USE,DEF}`` works exactly like
``FOR_EACH_SSA_{USE,DEF}_OPERAND``, except it will function on
either a statement or a ``PHI`` node.  These should be used when it is
appropriate but they are not quite as efficient as the individual
``FOR_EACH_PHI`` and ``FOR_EACH_SSA`` routines.

.. code-block:: c++

  FOR_EACH_PHI_OR_STMT_USE (use_operand_p, stmt, iter, flags)
    {
       my_code;
    }

  FOR_EACH_PHI_OR_STMT_DEF (def_operand_p, phi, iter, flags)
    {
       my_code;
    }

.. index:: Immediate Uses

Immediate Uses
^^^^^^^^^^^^^^

Immediate use information is now always available.  Using the immediate use
iterators, you may examine every use of any ``SSA_NAME``. For instance,
to change each use of ``ssa_var`` to ``ssa_var2`` and call fold_stmt on
each stmt after that is done:

.. code-block:: c++

    use_operand_p imm_use_p;
    imm_use_iterator iterator;
    tree ssa_var, stmt;

    FOR_EACH_IMM_USE_STMT (stmt, iterator, ssa_var)
      {
        FOR_EACH_IMM_USE_ON_STMT (imm_use_p, iterator)
          SET_USE (imm_use_p, ssa_var_2);
        fold_stmt (stmt);
      }

There are 2 iterators which can be used. ``FOR_EACH_IMM_USE_FAST`` is
used when the immediate uses are not changed, i.e., you are looking at the
uses, but not setting them.

If they do get changed, then care must be taken that things are not changed
under the iterators, so use the ``FOR_EACH_IMM_USE_STMT`` and
``FOR_EACH_IMM_USE_ON_STMT`` iterators.  They attempt to preserve the
sanity of the use list by moving all the uses for a statement into
a controlled position, and then iterating over those uses.  Then the
optimization can manipulate the stmt when all the uses have been
processed.  This is a little slower than the FAST version since it adds a
placeholder element and must sort through the list a bit for each statement.
This placeholder element must be also be removed if the loop is
terminated early; a destructor takes care of that when leaving the
``FOR_EACH_IMM_USE_STMT`` scope.

There are checks in ``verify_ssa`` which verify that the immediate use list
is up to date, as well as checking that an optimization didn't break from the
loop without using this macro.  It is safe to simply 'break'; from a
``FOR_EACH_IMM_USE_FAST`` traverse.

Some useful functions and macros:

* ``has_zero_uses (ssa_var)`` : Returns true if there are no uses of
  ``ssa_var``.

* ``has_single_use (ssa_var)`` : Returns true if there is only a
  single use of ``ssa_var``.

* ``single_imm_use (ssa_var, use_operand_p *ptr, tree *stmt)`` :
  Returns true if there is only a single use of ``ssa_var``, and also returns
  the use pointer and statement it occurs in, in the second and third parameters.

* ``num_imm_uses (ssa_var)`` : Returns the number of immediate uses of
  ``ssa_var``. It is better not to use this if possible since it simply
  utilizes a loop to count the uses.

* ``PHI_ARG_INDEX_FROM_USE (use_p)`` : Given a use within a ``PHI``
  node, return the index number for the use.  An assert is triggered if the use
  isn't located in a ``PHI`` node.

* ``USE_STMT (use_p)`` : Return the statement a use occurs in.

Note that uses are not put into an immediate use list until their statement is
actually inserted into the instruction stream via a ``bsi_*`` routine.

It is also still possible to utilize lazy updating of statements, but this
should be used only when absolutely required.  Both alias analysis and the
dominator optimizations currently do this.

When lazy updating is being used, the immediate use information is out of date
and cannot be used reliably.  Lazy updating is achieved by simply marking
statements modified via calls to ``gimple_set_modified`` instead of
``update_stmt``.  When lazy updating is no longer required, all the
modified statements must have ``update_stmt`` called in order to bring them
up to date.  This must be done before the optimization is finished, or
``verify_ssa`` will trigger an abort.

This is done with a simple loop over the instruction stream:

.. code-block:: c++

    block_stmt_iterator bsi;
    basic_block bb;
    FOR_EACH_BB (bb)
      {
        for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
          update_stmt_if_modified (bsi_stmt (bsi));
      }
