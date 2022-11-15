..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Statements

.. _statements:

Statements
**********

Most statements in GIMPLE are assignment statements, represented by
``GIMPLE_ASSIGN``.  No other C expressions can appear at statement level;
a reference to a volatile object is converted into a
``GIMPLE_ASSIGN``.

There are also several varieties of complex statements.

.. toctree::
  :maxdepth: 2


.. index:: Basic Statements

.. _basic-statements:

Basic Statements
^^^^^^^^^^^^^^^^

.. envvar:: ASM_EXPR

  Used to represent an inline assembly statement.  For an inline assembly
  statement like:

  .. code-block:: c++

    asm ("mov x, y");

  The ``ASM_STRING`` macro will return a ``STRING_CST`` node for
  ``"mov x, y"``.  If the original statement made use of the
  extended-assembly syntax, then ``ASM_OUTPUTS``,
  ``ASM_INPUTS``, and ``ASM_CLOBBERS`` will be the outputs, inputs,
  and clobbers for the statement, represented as ``STRING_CST`` nodes.
  The extended-assembly syntax looks like:

  .. code-block:: c++

    asm ("fsinx %1,%0" : "=f" (result) : "f" (angle));

  The first string is the ``ASM_STRING``, containing the instruction
  template.  The next two strings are the output and inputs, respectively;
  this statement has no clobbers.  As this example indicates, 'plain'
  assembly statements are merely a special case of extended assembly
  statements; they have no cv-qualifiers, outputs, inputs, or clobbers.
  All of the strings will be ``NUL`` -terminated, and will contain no
  embedded ``NUL`` -characters.

  If the assembly statement is declared ``volatile``, or if the
  statement was not an extended assembly statement, and is therefore
  implicitly volatile, then the predicate ``ASM_VOLATILE_P`` will hold
  of the ``ASM_EXPR``.

.. envvar:: DECL_EXPR

  Used to represent a local declaration.  The ``DECL_EXPR_DECL`` macro
  can be used to obtain the entity declared.  This declaration may be a
  ``LABEL_DECL``, indicating that the label declared is a local label.
  (As an extension, GCC allows the declaration of labels with scope.)  In
  C, this declaration may be a ``FUNCTION_DECL``, indicating the
  use of the GCC nested function extension.  For more information,
  see :ref:`functions`.

.. envvar:: LABEL_EXPR

  Used to represent a label.  The ``LABEL_DECL`` declared by this
  statement can be obtained with the ``LABEL_EXPR_LABEL`` macro.  The
  ``IDENTIFIER_NODE`` giving the name of the label can be obtained from
  the ``LABEL_DECL`` with ``DECL_NAME``.

.. envvar:: GOTO_EXPR

  Used to represent a ``goto`` statement.  The ``GOTO_DESTINATION`` will
  usually be a ``LABEL_DECL``.  However, if the 'computed goto' extension
  has been used, the ``GOTO_DESTINATION`` will be an arbitrary expression
  indicating the destination.  This expression will always have pointer type.

.. envvar:: RETURN_EXPR

  Used to represent a ``return`` statement.  Operand 0 represents the
  value to return.  It should either be the ``RESULT_DECL`` for the
  containing function, or a ``MODIFY_EXPR`` or ``INIT_EXPR``
  setting the function's ``RESULT_DECL``.  It will be
  ``NULL_TREE`` if the statement was just

  .. code-block:: c++

    return;

.. envvar:: LOOP_EXPR

  These nodes represent 'infinite' loops.  The ``LOOP_EXPR_BODY``
  represents the body of the loop.  It should be executed forever, unless
  an ``EXIT_EXPR`` is encountered.

.. envvar:: EXIT_EXPR

  These nodes represent conditional exits from the nearest enclosing
  ``LOOP_EXPR``.  The single operand is the condition; if it is
  nonzero, then the loop should be exited.  An ``EXIT_EXPR`` will only
  appear within a ``LOOP_EXPR``.

.. envvar:: SWITCH_EXPR

  Used to represent a ``switch`` statement.  The ``SWITCH_COND``
  is the expression on which the switch is occurring.  The
  ``SWITCH_BODY`` is the body of the switch statement.
  ``SWITCH_ALL_CASES_P`` is true if the switch includes a default
  label or the case label ranges cover all possible values of the
  condition expression.

  Note that ``TREE_TYPE`` for a ``SWITCH_EXPR`` represents the
  original type of switch expression as given in the source, before any
  compiler conversions, instead of the type of the switch expression
  itself (which is not meaningful).

.. envvar:: CASE_LABEL_EXPR

  Use to represent a ``case`` label, range of ``case`` labels, or a
  ``default`` label.  If ``CASE_LOW`` is ``NULL_TREE``, then this is a
  ``default`` label.  Otherwise, if ``CASE_HIGH`` is ``NULL_TREE``, then
  this is an ordinary ``case`` label.  In this case, ``CASE_LOW`` is
  an expression giving the value of the label.  Both ``CASE_LOW`` and
  ``CASE_HIGH`` are ``INTEGER_CST`` nodes.  These values will have
  the same type as the condition expression in the switch statement.

  Otherwise, if both ``CASE_LOW`` and ``CASE_HIGH`` are defined, the
  statement is a range of case labels.  Such statements originate with the
  extension that allows users to write things of the form:

  .. code-block:: c++

    case 2 ... 5:

  The first value will be ``CASE_LOW``, while the second will be
  ``CASE_HIGH``.

.. envvar:: DEBUG_BEGIN_STMT

  Marks the beginning of a source statement, for purposes of debug
  information generation.

.. index:: Blocks

.. _blocks:

Blocks
^^^^^^

Block scopes and the variables they declare in GENERIC are
expressed using the ``BIND_EXPR`` code, which in previous
versions of GCC was primarily used for the C statement-expression
extension.

Variables in a block are collected into ``BIND_EXPR_VARS`` in
declaration order through their ``TREE_CHAIN`` field.  Any runtime
initialization is moved out of ``DECL_INITIAL`` and into a
statement in the controlled block.  When gimplifying from C or C++,
this initialization replaces the ``DECL_STMT``.  These variables
will never require cleanups.  The scope of these variables is just the
body

Variable-length arrays (VLAs) complicate this process, as their size
often refers to variables initialized earlier in the block and their
initialization involves an explicit stack allocation.  To handle this,
we add an indirection and replace them with a pointer to stack space
allocated by means of ``alloca``.  In most cases, we also arrange
for this space to be reclaimed when the enclosing ``BIND_EXPR`` is
exited, the exception to this being when there is an explicit call to
``alloca`` in the source code, in which case the stack is left
depressed on exit of the ``BIND_EXPR``.

A C++ program will usually contain more ``BIND_EXPR`` s than
there are syntactic blocks in the source code, since several C++
constructs have implicit scopes associated with them.  On the
other hand, although the C++ front end uses pseudo-scopes to
handle cleanups for objects with destructors, these don't
translate into the GIMPLE form; multiple declarations at the same
level use the same ``BIND_EXPR``.

.. index:: Statement Sequences

.. _statement-sequences:

Statement Sequences
^^^^^^^^^^^^^^^^^^^

Multiple statements at the same nesting level are collected into
a ``STATEMENT_LIST``.  Statement lists are modified and
traversed using the interface in :samp:`tree-iterator.h`.

.. index:: Empty Statements

.. _empty-statements:

Empty Statements
^^^^^^^^^^^^^^^^

Whenever possible, statements with no effect are discarded.  But
if they are nested within another construct which cannot be
discarded for some reason, they are instead replaced with an
empty statement, generated by ``build_empty_stmt``.
Initially, all empty statements were shared, after the pattern of
the Java front end, but this caused a lot of trouble in practice.

An empty statement is represented as ``(void)0``.

.. index:: Jumps

.. _jumps:

Jumps
^^^^^

Other jumps are expressed by either ``GOTO_EXPR`` or
``RETURN_EXPR``.

The operand of a ``GOTO_EXPR`` must be either a label or a
variable containing the address to jump to.

The operand of a ``RETURN_EXPR`` is either ``NULL_TREE``,
``RESULT_DECL``, or a ``MODIFY_EXPR`` which sets the return
value.  It would be nice to move the ``MODIFY_EXPR`` into a
separate statement, but the special return semantics in
``expand_return`` make that difficult.  It may still happen in
the future, perhaps by moving most of that logic into
``expand_assignment``.

.. index:: Cleanups

.. _cleanups:

Cleanups
^^^^^^^^

Destructors for local C++ objects and similar dynamic cleanups are
represented in GIMPLE by a ``TRY_FINALLY_EXPR``.
``TRY_FINALLY_EXPR`` has two operands, both of which are a sequence
of statements to execute.  The first sequence is executed.  When it
completes the second sequence is executed.

The first sequence may complete in the following ways:

* Execute the last statement in the sequence and fall off the
  end.

* Execute a goto statement (``GOTO_EXPR``) to an ordinary
  label outside the sequence.

* Execute a return statement (``RETURN_EXPR``).

* Throw an exception.  This is currently not explicitly represented in
  GIMPLE.

The second sequence is not executed if the first sequence completes by
calling ``setjmp`` or ``exit`` or any other function that does
not return.  The second sequence is also not executed if the first
sequence completes via a non-local goto or a computed goto (in general
the compiler does not know whether such a goto statement exits the
first sequence or not, so we assume that it doesn't).

After the second sequence is executed, if it completes normally by
falling off the end, execution continues wherever the first sequence
would have continued, by falling off the end, or doing a goto, etc.

If the second sequence is an ``EH_ELSE_EXPR`` selector, then the
sequence in its first operand is used when the first sequence completes
normally, and that in its second operand is used for exceptional
cleanups, i.e., when an exception propagates out of the first sequence.

``TRY_FINALLY_EXPR`` complicates the flow graph, since the cleanup
needs to appear on every edge out of the controlled block; this
reduces the freedom to move code across these edges.  Therefore, the
EH lowering pass which runs before most of the optimization passes
eliminates these expressions by explicitly adding the cleanup to each
edge.  Rethrowing the exception is represented using ``RESX_EXPR``.

.. _openmp:

OpenMP
^^^^^^

.. index:: OMP_PARALLEL, OMP_FOR, OMP_SECTIONS, OMP_SINGLE, OMP_SECTION, OMP_MASTER, OMP_ORDERED, OMP_CRITICAL, OMP_RETURN, OMP_CONTINUE, OMP_ATOMIC, OMP_CLAUSE

All the statements starting with ``OMP_`` represent directives and
clauses used by the OpenMP API https://www.openmp.org.

.. envvar:: OMP_PARALLEL

  Represents ``#pragma omp parallel [clause1 ... clauseN]``. It
  has four operands:

  Operand ``OMP_PARALLEL_BODY`` is valid while in GENERIC and
  High GIMPLE forms.  It contains the body of code to be executed
  by all the threads.  During GIMPLE lowering, this operand becomes
  ``NULL`` and the body is emitted linearly after
  ``OMP_PARALLEL``.

  Operand ``OMP_PARALLEL_CLAUSES`` is the list of clauses
  associated with the directive.

  Operand ``OMP_PARALLEL_FN`` is created by
  ``pass_lower_omp``, it contains the ``FUNCTION_DECL``
  for the function that will contain the body of the parallel
  region.

  Operand ``OMP_PARALLEL_DATA_ARG`` is also created by
  ``pass_lower_omp``. If there are shared variables to be
  communicated to the children threads, this operand will contain
  the ``VAR_DECL`` that contains all the shared values and
  variables.

.. envvar:: OMP_FOR

  Represents ``#pragma omp for [clause1 ... clauseN]``.  It has
  six operands:

  Operand ``OMP_FOR_BODY`` contains the loop body.

  Operand ``OMP_FOR_CLAUSES`` is the list of clauses
  associated with the directive.

  Operand ``OMP_FOR_INIT`` is the loop initialization code of
  the form ``VAR = N1``.

  Operand ``OMP_FOR_COND`` is the loop conditional expression
  of the form ``VAR {<,>,<=,>=} N2``.

  Operand ``OMP_FOR_INCR`` is the loop index increment of the
  form ``VAR {+=,-=} INCR``.

  Operand ``OMP_FOR_PRE_BODY`` contains side effect code from
  operands ``OMP_FOR_INIT``, ``OMP_FOR_COND`` and
  ``OMP_FOR_INC``.  These side effects are part of the
  ``OMP_FOR`` block but must be evaluated before the start of
  loop body.

  The loop index variable ``VAR`` must be a signed integer variable,
  which is implicitly private to each thread.  Bounds
  ``N1`` and ``N2`` and the increment expression
  ``INCR`` are required to be loop invariant integer
  expressions that are evaluated without any synchronization. The
  evaluation order, frequency of evaluation and side effects are
  unspecified by the standard.

.. envvar:: OMP_SECTIONS

  Represents ``#pragma omp sections [clause1 ... clauseN]``.

  Operand ``OMP_SECTIONS_BODY`` contains the sections body,
  which in turn contains a set of ``OMP_SECTION`` nodes for
  each of the concurrent sections delimited by ``#pragma omp
  section``.

  Operand ``OMP_SECTIONS_CLAUSES`` is the list of clauses
  associated with the directive.

.. envvar:: OMP_SECTION

  Section delimiter for ``OMP_SECTIONS``.

.. envvar:: OMP_SINGLE

  Represents ``#pragma omp single``.

  Operand ``OMP_SINGLE_BODY`` contains the body of code to be
  executed by a single thread.

  Operand ``OMP_SINGLE_CLAUSES`` is the list of clauses
  associated with the directive.

.. envvar:: OMP_MASTER

  Represents ``#pragma omp master``.

  Operand ``OMP_MASTER_BODY`` contains the body of code to be
  executed by the master thread.

.. envvar:: OMP_ORDERED

  Represents ``#pragma omp ordered``.

  Operand ``OMP_ORDERED_BODY`` contains the body of code to be
  executed in the sequential order dictated by the loop index
  variable.

.. envvar:: OMP_CRITICAL

  Represents ``#pragma omp critical [name]``.

  Operand ``OMP_CRITICAL_BODY`` is the critical section.

  Operand ``OMP_CRITICAL_NAME`` is an optional identifier to
  label the critical section.

.. envvar:: OMP_RETURN

  This does not represent any OpenMP directive, it is an artificial
  marker to indicate the end of the body of an OpenMP. It is used
  by the flow graph (``tree-cfg.cc``) and OpenMP region
  building code (``omp-low.cc``).

.. envvar:: OMP_CONTINUE

  Similarly, this instruction does not represent an OpenMP
  directive, it is used by ``OMP_FOR`` (and similar codes) as well as
  ``OMP_SECTIONS`` to mark the place where the code needs to
  loop to the next iteration, or the next section, respectively.

  In some cases, ``OMP_CONTINUE`` is placed right before
  ``OMP_RETURN``.  But if there are cleanups that need to
  occur right after the looping body, it will be emitted between
  ``OMP_CONTINUE`` and ``OMP_RETURN``.

.. envvar:: OMP_ATOMIC

  Represents ``#pragma omp atomic``.

  Operand 0 is the address at which the atomic operation is to be
  performed.

  Operand 1 is the expression to evaluate.  The gimplifier tries
  three alternative code generation strategies.  Whenever possible,
  an atomic update built-in is used.  If that fails, a
  compare-and-swap loop is attempted.  If that also fails, a
  regular critical section around the expression is used.

.. envvar:: OMP_CLAUSE

  Represents clauses associated with one of the ``OMP_`` directives.
  Clauses are represented by separate subcodes defined in
  :samp:`tree.h`.  Clauses codes can be one of:
  ``OMP_CLAUSE_PRIVATE``, ``OMP_CLAUSE_SHARED``,
  ``OMP_CLAUSE_FIRSTPRIVATE``,
  ``OMP_CLAUSE_LASTPRIVATE``, ``OMP_CLAUSE_COPYIN``,
  ``OMP_CLAUSE_COPYPRIVATE``, ``OMP_CLAUSE_IF``,
  ``OMP_CLAUSE_NUM_THREADS``, ``OMP_CLAUSE_SCHEDULE``,
  ``OMP_CLAUSE_NOWAIT``, ``OMP_CLAUSE_ORDERED``,
  ``OMP_CLAUSE_DEFAULT``, ``OMP_CLAUSE_REDUCTION``,
  ``OMP_CLAUSE_COLLAPSE``, ``OMP_CLAUSE_UNTIED``,
  ``OMP_CLAUSE_FINAL``, and ``OMP_CLAUSE_MERGEABLE``.  Each code
  represents the corresponding OpenMP clause.

  Clauses associated with the same directive are chained together
  via ``OMP_CLAUSE_CHAIN``. Those clauses that accept a list
  of variables are restricted to exactly one, accessed with
  ``OMP_CLAUSE_VAR``.  Therefore, multiple variables under the
  same clause ``C`` need to be represented as multiple ``C`` clauses
  chained together.  This facilitates adding new clauses during
  compilation.

.. _openacc:

OpenACC
^^^^^^^

.. index:: OACC_CACHE, OACC_DATA, OACC_DECLARE, OACC_ENTER_DATA, OACC_EXIT_DATA, OACC_HOST_DATA, OACC_KERNELS, OACC_LOOP, OACC_PARALLEL, OACC_SERIAL, OACC_UPDATE

All the statements starting with ``OACC_`` represent directives and
clauses used by the OpenACC API https://www.openacc.org.

.. envvar:: OACC_CACHE

  Represents ``#pragma acc cache (var ...)``.

.. envvar:: OACC_DATA

  Represents ``#pragma acc data [clause1 ... clauseN]``.

.. envvar:: OACC_DECLARE

  Represents ``#pragma acc declare [clause1 ... clauseN]``.

.. envvar:: OACC_ENTER_DATA

  Represents ``#pragma acc enter data [clause1 ... clauseN]``.

.. envvar:: OACC_EXIT_DATA

  Represents ``#pragma acc exit data [clause1 ... clauseN]``.

.. envvar:: OACC_HOST_DATA

  Represents ``#pragma acc host_data [clause1 ... clauseN]``.

.. envvar:: OACC_KERNELS

  Represents ``#pragma acc kernels [clause1 ... clauseN]``.

.. envvar:: OACC_LOOP

  Represents ``#pragma acc loop [clause1 ... clauseN]``.

  See the description of the ``OMP_FOR`` code.

.. envvar:: OACC_PARALLEL

  Represents ``#pragma acc parallel [clause1 ... clauseN]``.

.. envvar:: OACC_SERIAL

  Represents ``#pragma acc serial [clause1 ... clauseN]``.

.. envvar:: OACC_UPDATE

  Represents ``#pragma acc update [clause1 ... clauseN]``.
