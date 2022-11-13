..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _tree-ssa-passes:

Tree SSA passes
***************

The following briefly describes the Tree optimization passes that are
run after gimplification and what source files they are located in.

* Remove useless statements

  This pass is an extremely simple sweep across the gimple code in which
  we identify obviously dead code and remove it.  Here we do things like
  simplify ``if`` statements with constant conditions, remove
  exception handling constructs surrounding code that obviously cannot
  throw, remove lexical bindings that contain no variables, and other
  assorted simplistic cleanups.  The idea is to get rid of the obvious
  stuff quickly rather than wait until later when it's more work to get
  rid of it.  This pass is located in :samp:`tree-cfg.cc` and described by
  ``pass_remove_useless_stmts``.

* OpenMP lowering

  If OpenMP generation (:option:`-fopenmp`) is enabled, this pass lowers
  OpenMP constructs into GIMPLE.

  Lowering of OpenMP constructs involves creating replacement
  expressions for local variables that have been mapped using data
  sharing clauses, exposing the control flow of most synchronization
  directives and adding region markers to facilitate the creation of the
  control flow graph.  The pass is located in :samp:`omp-low.cc` and is
  described by ``pass_lower_omp``.

* OpenMP expansion

  If OpenMP generation (:option:`-fopenmp`) is enabled, this pass expands
  parallel regions into their own functions to be invoked by the thread
  library.  The pass is located in :samp:`omp-low.cc` and is described by
  ``pass_expand_omp``.

* Lower control flow

  This pass flattens ``if`` statements (``COND_EXPR``)
  and moves lexical bindings (``BIND_EXPR``) out of line.  After
  this pass, all ``if`` statements will have exactly two ``goto``
  statements in its ``then`` and ``else`` arms.  Lexical binding
  information for each statement will be found in ``TREE_BLOCK`` rather
  than being inferred from its position under a ``BIND_EXPR``.  This
  pass is found in :samp:`gimple-low.cc` and is described by
  ``pass_lower_cf``.

* Lower exception handling control flow

  This pass decomposes high-level exception handling constructs
  (``TRY_FINALLY_EXPR`` and ``TRY_CATCH_EXPR``) into a form
  that explicitly represents the control flow involved.  After this
  pass, ``lookup_stmt_eh_region`` will return a non-negative
  number for any statement that may have EH control flow semantics;
  examine ``tree_can_throw_internal`` or ``tree_can_throw_external``
  for exact semantics.  Exact control flow may be extracted from
  ``foreach_reachable_handler``.  The EH region nesting tree is defined
  in :samp:`except.h` and built in :samp:`except.cc`.  The lowering pass
  itself is in :samp:`tree-eh.cc` and is described by ``pass_lower_eh``.

* Build the control flow graph

  This pass decomposes a function into basic blocks and creates all of
  the edges that connect them.  It is located in :samp:`tree-cfg.cc` and
  is described by ``pass_build_cfg``.

* Find all referenced variables

  This pass walks the entire function and collects an array of all
  variables referenced in the function, ``referenced_vars``.  The
  index at which a variable is found in the array is used as a UID
  for the variable within this function.  This data is needed by the
  SSA rewriting routines.  The pass is located in :samp:`tree-dfa.cc`
  and is described by ``pass_referenced_vars``.

* Enter static single assignment form

  This pass rewrites the function such that it is in SSA form.  After
  this pass, all ``is_gimple_reg`` variables will be referenced by
  ``SSA_NAME``, and all occurrences of other variables will be
  annotated with ``VDEFS`` and ``VUSES`` ; PHI nodes will have
  been inserted as necessary for each basic block.  This pass is
  located in :samp:`tree-ssa.cc` and is described by ``pass_build_ssa``.

* Warn for uninitialized variables

  This pass scans the function for uses of ``SSA_NAME`` s that
  are fed by default definition.  For non-parameter variables, such
  uses are uninitialized.  The pass is run twice, before and after
  optimization (if turned on).  In the first pass we only warn for uses that are
  positively uninitialized; in the second pass we warn for uses that
  are possibly uninitialized.  The pass is located in :samp:`tree-ssa.cc`
  and is defined by ``pass_early_warn_uninitialized`` and
  ``pass_late_warn_uninitialized``.

* Dead code elimination

  This pass scans the function for statements without side effects whose
  result is unused.  It does not do memory life analysis, so any value
  that is stored in memory is considered used.  The pass is run multiple
  times throughout the optimization process.  It is located in
  :samp:`tree-ssa-dce.cc` and is described by ``pass_dce``.

* Dominator optimizations

  This pass performs trivial dominator-based copy and constant propagation,
  expression simplification, and jump threading.  It is run multiple times
  throughout the optimization process.  It is located in :samp:`tree-ssa-dom.cc`
  and is described by ``pass_dominator``.

* Forward propagation of single-use variables

  This pass attempts to remove redundant computation by substituting
  variables that are used once into the expression that uses them and
  seeing if the result can be simplified.  It is located in
  :samp:`tree-ssa-forwprop.cc` and is described by ``pass_forwprop``.

* Copy Renaming

  This pass attempts to change the name of compiler temporaries involved in
  copy operations such that SSA->normal can coalesce the copy away.  When compiler
  temporaries are copies of user variables, it also renames the compiler
  temporary to the user variable resulting in better use of user symbols.  It is
  located in :samp:`tree-ssa-copyrename.c` and is described by
  ``pass_copyrename``.

* PHI node optimizations

  This pass recognizes forms of PHI inputs that can be represented as
  conditional expressions and rewrites them into straight line code.
  It is located in :samp:`tree-ssa-phiopt.cc` and is described by
  ``pass_phiopt``.

* May-alias optimization

  This pass performs a flow sensitive SSA-based points-to analysis.
  The resulting may-alias, must-alias, and escape analysis information
  is used to promote variables from in-memory addressable objects to
  non-aliased variables that can be renamed into SSA form.  We also
  update the ``VDEF`` / ``VUSE`` memory tags for non-renameable
  aggregates so that we get fewer false kills.  The pass is located
  in :samp:`tree-ssa-alias.cc` and is described by ``pass_may_alias``.

  Interprocedural points-to information is located in
  :samp:`tree-ssa-structalias.cc` and described by ``pass_ipa_pta``.

* Profiling

  This pass instruments the function in order to collect runtime block
  and value profiling data.  Such data may be fed back into the compiler
  on a subsequent run so as to allow optimization based on expected
  execution frequencies.  The pass is located in :samp:`tree-profile.cc` and
  is described by ``pass_ipa_tree_profile``.

* Static profile estimation

  This pass implements series of heuristics to guess propababilities
  of branches.  The resulting predictions are turned into edge profile
  by propagating branches across the control flow graphs.
  The pass is located in :samp:`tree-profile.cc` and is described by
  ``pass_profile``.

* Lower complex arithmetic

  This pass rewrites complex arithmetic operations into their component
  scalar arithmetic operations.  The pass is located in :samp:`tree-complex.cc`
  and is described by ``pass_lower_complex``.

* Scalar replacement of aggregates

  This pass rewrites suitable non-aliased local aggregate variables into
  a set of scalar variables.  The resulting scalar variables are
  rewritten into SSA form, which allows subsequent optimization passes
  to do a significantly better job with them.  The pass is located in
  :samp:`tree-sra.cc` and is described by ``pass_sra``.

* Dead store elimination

  This pass eliminates stores to memory that are subsequently overwritten
  by another store, without any intervening loads.  The pass is located
  in :samp:`tree-ssa-dse.cc` and is described by ``pass_dse``.

* Tail recursion elimination

  This pass transforms tail recursion into a loop.  It is located in
  :samp:`tree-tailcall.cc` and is described by ``pass_tail_recursion``.

* Forward store motion

  This pass sinks stores and assignments down the flowgraph closer to their
  use point.  The pass is located in :samp:`tree-ssa-sink.cc` and is
  described by ``pass_sink_code``.

* Partial redundancy elimination

  This pass eliminates partially redundant computations, as well as
  performing load motion.  The pass is located in :samp:`tree-ssa-pre.cc`
  and is described by ``pass_pre``.

  Just before partial redundancy elimination, if
  :option:`-funsafe-math-optimizations` is on, GCC tries to convert
  divisions to multiplications by the reciprocal.  The pass is located
  in :samp:`tree-ssa-math-opts.cc` and is described by
  ``pass_cse_reciprocal``.

* Full redundancy elimination

  This is a simpler form of PRE that only eliminates redundancies that
  occur on all paths.  It is located in :samp:`tree-ssa-pre.cc` and
  described by ``pass_fre``.

* Loop optimization

  The main driver of the pass is placed in :samp:`tree-ssa-loop.cc`
  and described by ``pass_loop``.

  The optimizations performed by this pass are:

  Loop invariant motion.  This pass moves only invariants that
  would be hard to handle on RTL level (function calls, operations that expand to
  nontrivial sequences of insns).  With :option:`-funswitch-loops` it also moves
  operands of conditions that are invariant out of the loop, so that we can use
  just trivial invariantness analysis in loop unswitching.  The pass also includes
  store motion.  The pass is implemented in :samp:`tree-ssa-loop-im.cc`.

  Canonical induction variable creation.  This pass creates a simple counter
  for number of iterations of the loop and replaces the exit condition of the
  loop using it, in case when a complicated analysis is necessary to determine
  the number of iterations.  Later optimizations then may determine the number
  easily.  The pass is implemented in :samp:`tree-ssa-loop-ivcanon.cc`.

  Induction variable optimizations.  This pass performs standard induction
  variable optimizations, including strength reduction, induction variable
  merging and induction variable elimination.  The pass is implemented in
  :samp:`tree-ssa-loop-ivopts.cc`.

  Loop unswitching.  This pass moves the conditional jumps that are invariant
  out of the loops.  To achieve this, a duplicate of the loop is created for
  each possible outcome of conditional jump(s).  The pass is implemented in
  :samp:`tree-ssa-loop-unswitch.cc`.

  Loop splitting.  If a loop contains a conditional statement that is
  always true for one part of the iteration space and false for the other
  this pass splits the loop into two, one dealing with one side the other
  only with the other, thereby removing one inner-loop conditional.  The
  pass is implemented in :samp:`tree-ssa-loop-split.cc`.

  The optimizations also use various utility functions contained in
  :samp:`tree-ssa-loop-manip.cc`, :samp:`cfgloop.cc`, :samp:`cfgloopanal.cc` and
  :samp:`cfgloopmanip.cc`.

  Vectorization.  This pass transforms loops to operate on vector types
  instead of scalar types.  Data parallelism across loop iterations is exploited
  to group data elements from consecutive iterations into a vector and operate
  on them in parallel.  Depending on available target support the loop is
  conceptually unrolled by a factor ``VF`` (vectorization factor), which is
  the number of elements operated upon in parallel in each iteration, and the
  ``VF`` copies of each scalar operation are fused to form a vector operation.
  Additional loop transformations such as peeling and versioning may take place
  to align the number of iterations, and to align the memory accesses in the
  loop.
  The pass is implemented in :samp:`tree-vectorizer.cc` (the main driver),
  :samp:`tree-vect-loop.cc` and :samp:`tree-vect-loop-manip.cc` (loop specific parts
  and general loop utilities), :samp:`tree-vect-slp` (loop-aware SLP
  functionality), :samp:`tree-vect-stmts.cc`, :samp:`tree-vect-data-refs.cc` and
  :samp:`tree-vect-slp-patterns.cc` containing the SLP pattern matcher.
  Analysis of data references is in :samp:`tree-data-ref.cc`.

  SLP Vectorization.  This pass performs vectorization of straight-line code. The
  pass is implemented in :samp:`tree-vectorizer.cc` (the main driver),
  :samp:`tree-vect-slp.cc`, :samp:`tree-vect-stmts.cc` and
  :samp:`tree-vect-data-refs.cc`.

  Autoparallelization.  This pass splits the loop iteration space to run
  into several threads.  The pass is implemented in :samp:`tree-parloops.cc`.

  Graphite is a loop transformation framework based on the polyhedral
  model.  Graphite stands for Gimple Represented as Polyhedra.  The
  internals of this infrastructure are documented in
  https://gcc.gnu.org/wiki/Graphite.  The passes working on
  this representation are implemented in the various :samp:`graphite-*`
  files.

* Tree level if-conversion for vectorizer

  This pass applies if-conversion to simple loops to help vectorizer.
  We identify if convertible loops, if-convert statements and merge
  basic blocks in one big block.  The idea is to present loop in such
  form so that vectorizer can have one to one mapping between statements
  and available vector operations.  This pass is located in
  :samp:`tree-if-conv.cc` and is described by ``pass_if_conversion``.

* Conditional constant propagation

  This pass relaxes a lattice of values in order to identify those
  that must be constant even in the presence of conditional branches.
  The pass is located in :samp:`tree-ssa-ccp.cc` and is described
  by ``pass_ccp``.

  A related pass that works on memory loads and stores, and not just
  register values, is located in :samp:`tree-ssa-ccp.cc` and described by
  ``pass_store_ccp``.

* Conditional copy propagation

  This is similar to constant propagation but the lattice of values is
  the 'copy-of' relation.  It eliminates redundant copies from the
  code.  The pass is located in :samp:`tree-ssa-copy.cc` and described by
  ``pass_copy_prop``.

  A related pass that works on memory copies, and not just register
  copies, is located in :samp:`tree-ssa-copy.cc` and described by
  ``pass_store_copy_prop``.

* Value range propagation

  This transformation is similar to constant propagation but
  instead of propagating single constant values, it propagates
  known value ranges.  The implementation is based on Patterson's
  range propagation algorithm (Accurate Static Branch Prediction by
  Value Range Propagation, J. R. C. Patterson, PLDI '95).  In
  contrast to Patterson's algorithm, this implementation does not
  propagate branch probabilities nor it uses more than a single
  range per SSA name. This means that the current implementation
  cannot be used for branch prediction (though adapting it would
  not be difficult).  The pass is located in :samp:`tree-vrp.cc` and is
  described by ``pass_vrp``.

* Folding built-in functions

  This pass simplifies built-in functions, as applicable, with constant
  arguments or with inferable string lengths.  It is located in
  :samp:`tree-ssa-ccp.cc` and is described by ``pass_fold_builtins``.

* Split critical edges

  This pass identifies critical edges and inserts empty basic blocks
  such that the edge is no longer critical.  The pass is located in
  :samp:`tree-cfg.cc` and is described by ``pass_split_crit_edges``.

* Control dependence dead code elimination

  This pass is a stronger form of dead code elimination that can
  eliminate unnecessary control flow statements.   It is located
  in :samp:`tree-ssa-dce.cc` and is described by ``pass_cd_dce``.

* Tail call elimination

  This pass identifies function calls that may be rewritten into
  jumps.  No code transformation is actually applied here, but the
  data and control flow problem is solved.  The code transformation
  requires target support, and so is delayed until RTL.  In the
  meantime ``CALL_EXPR_TAILCALL`` is set indicating the possibility.
  The pass is located in :samp:`tree-tailcall.cc` and is described by
  ``pass_tail_calls``.  The RTL transformation is handled by
  ``fixup_tail_calls`` in :samp:`calls.cc`.

* Warn for function return without value

  For non-void functions, this pass locates return statements that do
  not specify a value and issues a warning.  Such a statement may have
  been injected by falling off the end of the function.  This pass is
  run last so that we have as much time as possible to prove that the
  statement is not reachable.  It is located in :samp:`tree-cfg.cc` and
  is described by ``pass_warn_function_return``.

* Leave static single assignment form

  This pass rewrites the function such that it is in normal form.  At
  the same time, we eliminate as many single-use temporaries as possible,
  so the intermediate language is no longer GIMPLE, but GENERIC.  The
  pass is located in :samp:`tree-outof-ssa.cc` and is described by
  ``pass_del_ssa``.

* Merge PHI nodes that feed into one another

  This is part of the CFG cleanup passes.  It attempts to join PHI nodes
  from a forwarder CFG block into another block with PHI nodes.  The
  pass is located in :samp:`tree-cfgcleanup.cc` and is described by
  ``pass_merge_phi``.

* Return value optimization

  If a function always returns the same local variable, and that local
  variable is an aggregate type, then the variable is replaced with the
  return value for the function (i.e., the function's DECL_RESULT).  This
  is equivalent to the C++ named return value optimization applied to
  GIMPLE.  The pass is located in :samp:`tree-nrv.cc` and is described by
  ``pass_nrv``.

* Return slot optimization

  If a function returns a memory object and is called as ``var =
  foo()``, this pass tries to change the call so that the address of
  ``var`` is sent to the caller to avoid an extra memory copy.  This
  pass is located in ``tree-nrv.cc`` and is described by
  ``pass_return_slot``.

* Optimize calls to ``__builtin_object_size``

  This is a propagation pass similar to CCP that tries to remove calls
  to ``__builtin_object_size`` when the size of the object can be
  computed at compile-time.  This pass is located in
  :samp:`tree-object-size.cc` and is described by
  ``pass_object_sizes``.

* Loop invariant motion

  This pass removes expensive loop-invariant computations out of loops.
  The pass is located in :samp:`tree-ssa-loop.cc` and described by
  ``pass_lim``.

* Loop nest optimizations

  This is a family of loop transformations that works on loop nests.  It
  includes loop interchange, scaling, skewing and reversal and they are
  all geared to the optimization of data locality in array traversals
  and the removal of dependencies that hamper optimizations such as loop
  parallelization and vectorization.  The pass is located in
  :samp:`tree-loop-linear.c` and described by
  ``pass_linear_transform``.

* Removal of empty loops

  This pass removes loops with no code in them.  The pass is located in
  :samp:`tree-ssa-loop-ivcanon.cc` and described by
  ``pass_empty_loop``.

* Unrolling of small loops

  This pass completely unrolls loops with few iterations.  The pass
  is located in :samp:`tree-ssa-loop-ivcanon.cc` and described by
  ``pass_complete_unroll``.

* Predictive commoning

  This pass makes the code reuse the computations from the previous
  iterations of the loops, especially loads and stores to memory.
  It does so by storing the values of these computations to a bank
  of temporary variables that are rotated at the end of loop.  To avoid
  the need for this rotation, the loop is then unrolled and the copies
  of the loop body are rewritten to use the appropriate version of
  the temporary variable.  This pass is located in :samp:`tree-predcom.cc`
  and described by ``pass_predcom``.

* Array prefetching

  This pass issues prefetch instructions for array references inside
  loops.  The pass is located in :samp:`tree-ssa-loop-prefetch.cc` and
  described by ``pass_loop_prefetch``.

* Reassociation

  This pass rewrites arithmetic expressions to enable optimizations that
  operate on them, like redundancy elimination and vectorization.  The
  pass is located in :samp:`tree-ssa-reassoc.cc` and described by
  ``pass_reassoc``.

* Optimization of ``stdarg`` functions

  This pass tries to avoid the saving of register arguments into the
  stack on entry to ``stdarg`` functions.  If the function doesn't
  use any ``va_start`` macros, no registers need to be saved.  If
  ``va_start`` macros are used, the ``va_list`` variables don't
  escape the function, it is only necessary to save registers that will
  be used in ``va_arg`` macros.  For instance, if ``va_arg`` is
  only used with integral types in the function, floating point
  registers don't need to be saved.  This pass is located in
  ``tree-stdarg.cc`` and described by ``pass_stdarg``.