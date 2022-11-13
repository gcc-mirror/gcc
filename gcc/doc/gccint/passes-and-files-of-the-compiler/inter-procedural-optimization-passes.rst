..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: IPA passes, inter-procedural optimization passes

.. _ipa-passes:

Inter-procedural optimization passes
************************************

The inter-procedural optimization (IPA) passes use call graph
information to perform transformations across function boundaries.
IPA is a critical part of link-time optimization (LTO) and
whole-program (WHOPR) optimization, and these passes are structured
with the needs of LTO and WHOPR in mind by dividing their operations
into stages.  For detailed discussion of the LTO/WHOPR IPA pass stages
and interfaces, see :ref:`IPA`.

The following briefly describes the inter-procedural optimization (IPA)
passes, which are split into small IPA passes, regular IPA passes,
and late IPA passes, according to the LTO/WHOPR processing model.

.. toctree::
  :maxdepth: 2


.. index:: small IPA passes

.. _small-ipa-passes:

Small IPA passes
^^^^^^^^^^^^^^^^

A small IPA pass is a pass derived from ``simple_ipa_opt_pass``.
As described in :ref:`IPA`, it does everything at once and
defines only the *Execute* stage.  During this
stage it accesses and modifies the function bodies.
No ``generate_summary``, ``read_summary``, or ``write_summary``
hooks are defined.

* IPA free lang data

  This pass frees resources that are used by the front end but are
  not needed once it is done.  It is located in :samp:`tree.cc` and is described by
  ``pass_ipa_free_lang_data``.

* IPA function and variable visibility

  This is a local function pass handling visibilities of all symbols.  This
  happens before LTO streaming, so :option:`-fwhole-program` should be ignored
  at this level.  It is located in :samp:`ipa-visibility.cc` and is described by
  ``pass_ipa_function_and_variable_visibility``.

* IPA remove symbols

  This pass performs reachability analysis and reclaims all unreachable nodes.
  It is located in :samp:`passes.cc` and is described by
  ``pass_ipa_remove_symbols``.

* IPA OpenACC

  This is a pass group for OpenACC processing.  It is located in
  :samp:`tree-ssa-loop.cc` and is described by ``pass_ipa_oacc``.

* IPA points-to analysis

  This is a tree-based points-to analysis pass. The idea behind this analyzer
  is to generate set constraints from the program, then solve the resulting
  constraints in order to generate the points-to sets.  It is located in
  :samp:`tree-ssa-structalias.cc` and is described by ``pass_ipa_pta``.

* IPA OpenACC kernels

  This is a pass group for processing OpenACC kernels regions.  It is a
  subpass of the IPA OpenACC pass group that runs on offloaded functions
  containing OpenACC kernels loops.  It is located in
  :samp:`tree-ssa-loop.cc` and is described by
  ``pass_ipa_oacc_kernels``.

* Target clone

  This is a pass for parsing functions with multiple target attributes.
  It is located in :samp:`multiple_target.cc` and is described by
  ``pass_target_clone``.

* IPA auto profile

  This pass uses AutoFDO profiling data to annotate the control flow graph.
  It is located in :samp:`auto-profile.cc` and is described by
  ``pass_ipa_auto_profile``.

* IPA tree profile

  This pass does profiling for all functions in the call graph.
  It calculates branch
  probabilities and basic block execution counts. It is located
  in :samp:`tree-profile.cc` and is described by ``pass_ipa_tree_profile``.

* IPA free function summary

  This pass is a small IPA pass when argument ``small_p`` is true.
  It releases inline function summaries and call summaries.
  It is located in :samp:`ipa-fnsummary.cc` and is described by
  ``pass_ipa_free_free_fn_summary``.

* IPA increase alignment

  This pass increases the alignment of global arrays to improve
  vectorization. It is located in :samp:`tree-vectorizer.cc`
  and is described by ``pass_ipa_increase_alignment``.

* IPA transactional memory

  This pass is for transactional memory support.
  It is located in :samp:`trans-mem.cc` and is described by
  ``pass_ipa_tm``.

* IPA lower emulated TLS

  This pass lowers thread-local storage (TLS) operations
  to emulation functions provided by libgcc.
  It is located in :samp:`tree-emutls.cc` and is described by
  ``pass_ipa_lower_emutls``.

.. index:: regular IPA passes

.. _regular-ipa-passes:

Regular IPA passes
^^^^^^^^^^^^^^^^^^

A regular IPA pass is a pass derived from ``ipa_opt_pass_d`` that
is executed in WHOPR compilation. Regular IPA passes may have summary
hooks implemented in any of the LGEN, WPA or LTRANS stages (see :ref:`ipa`).

* IPA whole program visibility

  This pass performs various optimizations involving symbol visibility
  with :option:`-fwhole-program`, including symbol privatization,
  discovering local functions, and dismantling comdat groups.  It is
  located in :samp:`ipa-visibility.cc` and is described by
  ``pass_ipa_whole_program_visibility``.

* IPA profile

  The IPA profile pass propagates profiling frequencies across the call
  graph.  It is located in :samp:`ipa-profile.cc` and is described by
  ``pass_ipa_profile``.

* IPA identical code folding

  This is the inter-procedural identical code folding pass.
  The goal of this transformation is to discover functions
  and read-only variables that have exactly the same semantics.  It is
  located in :samp:`ipa-icf.cc` and is described by ``pass_ipa_icf``.

* IPA devirtualization

  This pass performs speculative devirtualization based on the type
  inheritance graph.  When a polymorphic call has only one likely target
  in the unit, it is turned into a speculative call. It is located in
  :samp:`ipa-devirt.cc` and is described by ``pass_ipa_devirt``.

* IPA constant propagation

  The goal of this pass is to discover functions that are always invoked
  with some arguments with the same known constant values and to modify
  the functions accordingly.  It can also do partial specialization and
  type-based devirtualization.  It is located in :samp:`ipa-cp.cc` and is
  described by ``pass_ipa_cp``.

* IPA scalar replacement of aggregates

  This pass can replace an aggregate parameter with a set of other parameters
  representing part of the original, turning those passed by reference
  into new ones which pass the value directly.  It also removes unused
  function return values and unused function parameters.  This pass is
  located in :samp:`ipa-sra.cc` and is described by ``pass_ipa_sra``.

* IPA constructor/destructor merge

  This pass merges multiple constructors and destructors for static
  objects into single functions.  It's only run at LTO time unless the
  target doesn't support constructors and destructors natively.  The
  pass is located in :samp:`ipa.cc` and is described by
  ``pass_ipa_cdtor_merge``.

* IPA function summary

  This pass provides function analysis for inter-procedural passes.
  It collects estimates of function body size, execution time, and frame
  size for each function.  It also estimates information about function
  calls: call statement size, time and how often the parameters change
  for each call.  It is located in :samp:`ipa-fnsummary.cc` and is
  described by ``pass_ipa_fn_summary``.

* IPA inline

  The IPA inline pass handles function inlining with whole-program
  knowledge. Small functions that are candidates for inlining are
  ordered in increasing badness, bounded by unit growth parameters.
  Unreachable functions are removed from the call graph.  Functions called
  once and not exported from the unit are inlined.  This pass is located in
  :samp:`ipa-inline.cc` and is described by ``pass_ipa_inline``.

* IPA pure/const analysis

  This pass marks functions as being either const (``TREE_READONLY``) or
  pure (``DECL_PURE_P``).  The per-function information is produced
  by ``pure_const_generate_summary``, then the global information is computed
  by performing a transitive closure over the call graph.   It is located in
  :samp:`ipa-pure-const.cc` and is described by ``pass_ipa_pure_const``.

* IPA free function summary

  This pass is a regular IPA pass when argument ``small_p`` is false.
  It releases inline function summaries and call summaries.
  It is located in :samp:`ipa-fnsummary.cc` and is described by
  ``pass_ipa_free_fn_summary``.

* IPA reference

  This pass gathers information about how variables whose scope is
  confined to the compilation unit are used.  It is located in
  :samp:`ipa-reference.cc` and is described by ``pass_ipa_reference``.

* IPA single use

  This pass checks whether variables are used by a single function.
  It is located in :samp:`ipa.cc` and is described by
  ``pass_ipa_single_use``.

* IPA comdats

  This pass looks for static symbols that are used exclusively
  within one comdat group, and moves them into that comdat group. It is
  located in :samp:`ipa-comdats.cc` and is described by
  ``pass_ipa_comdats``.

.. index:: late IPA passes

.. _late-ipa-passes:

Late IPA passes
^^^^^^^^^^^^^^^

Late IPA passes are simple IPA passes executed after
the regular passes.  In WHOPR mode the passes are executed after
partitioning and thus see just parts of the compiled unit.

* Materialize all clones

  Once all functions from compilation unit are in memory, produce all clones
  and update all calls.  It is located in :samp:`ipa.cc` and is described by
  ``pass_materialize_all_clones``.

* IPA points-to analysis

  Points-to analysis; this is the same as the points-to-analysis pass
  run with the small IPA passes (see :ref:`small-ipa-passes`).

* OpenMP simd clone

  This is the OpenMP constructs' SIMD clone pass.  It creates the appropriate
  SIMD clones for functions tagged as elemental SIMD functions.
  It is located in :samp:`omp-simd-clone.cc` and is described by
  ``pass_omp_simd_clone``.