..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: developer options, debugging GCC, debug dump options, dump options, compilation statistics

.. _developer-options:

GCC Developer Options
*********************

This section describes command-line options that are primarily of
interest to GCC developers, including options to support compiler
testing and investigation of compiler bugs and compile-time
performance problems.  This includes options that produce debug dumps
at various points in the compilation; that print statistics such as
memory use and execution time; and that print information about GCC's
configuration, such as where it searches for libraries.  You should
rarely need to use any of these options for ordinary compilation and
linking tasks.

Many developer options that cause GCC to dump output to a file take an
optional :samp:`={filename}` suffix. You can specify :samp:`stdout`
or :samp:`-` to dump to standard output, and :samp:`stderr` for standard
error.

If :samp:`={filename}` is omitted, a default dump file name is
constructed by concatenating the base dump file name, a pass number,
phase letter, and pass name.  The base dump file name is the name of
output file produced by the compiler if explicitly specified and not
an executable; otherwise it is the source file name.
The pass number is determined by the order passes are registered with
the compiler's pass manager.
This is generally the same as the order of execution, but passes
registered by plugins, target-specific passes, or passes that are
otherwise registered late are numbered higher than the pass named
:samp:`final`, even if they are executed earlier.  The phase letter is
one of :samp:`i` (inter-procedural analysis), :samp:`l`
(language-specific), :samp:`r` (RTL), or :samp:`t` (tree).
The files are created in the directory of the output file.

.. option:: -fcallgraph-info, -fcallgraph-info={MARKERS}

  Makes the compiler output callgraph information for the program, on a
  per-object-file basis.  The information is generated in the common VCG
  format.  It can be decorated with additional, per-node and/or per-edge
  information, if a list of comma-separated markers is additionally
  specified.  When the ``su`` marker is specified, the callgraph is
  decorated with stack usage information; it is equivalent to
  :option:`-fstack-usage`.  When the ``da`` marker is specified, the
  callgraph is decorated with information about dynamically allocated
  objects.

  When compiling with :option:`-flto`, no callgraph information is output
  along with the object file.  At LTO link time, :option:`-fcallgraph-info`
  may generate multiple callgraph information files next to intermediate
  LTO output files.

.. index:: fdump-rtl-pass

.. option:: -dletters, -fdump-rtl-pass, -fdump-rtl-pass={filename}

  Says to make debugging dumps during compilation at times specified by
  :samp:`{letters}`.  This is used for debugging the RTL-based passes of the
  compiler.

  Some :option:`-dletters` switches have different meaning when
  :option:`-E` is used for preprocessing.  See :ref:`preprocessor-options`,
  for information about preprocessor-specific dump options.

  Debug dumps can be enabled with a :option:`-fdump-rtl` switch or some
  :option:`-d` option :samp:`{letters}`.  Here are the possible
  letters for use in :samp:`{pass}` and :samp:`{letters}`, and their meanings:

  .. option:: -fdump-rtl-alignments

    Dump after branch alignments have been computed.

  .. option:: -fdump-rtl-asmcons

    Dump after fixing rtl statements that have unsatisfied in/out constraints.

  .. option:: -fdump-rtl-auto_inc_dec

    Dump after auto-inc-dec discovery.  This pass is only run on
    architectures that have auto inc or auto dec instructions.

  .. option:: -fdump-rtl-barriers

    Dump after cleaning up the barrier instructions.

  .. option:: -fdump-rtl-bbpart

    Dump after partitioning hot and cold basic blocks.

  .. option:: -fdump-rtl-bbro

    Dump after block reordering.

  .. option:: -fdump-rtl-btl1, -fdump-rtl-btl2

    :option:`-fdump-rtl-btl1` and :option:`-fdump-rtl-btl2` enable dumping
    after the two branch
    target load optimization passes.

  .. option:: -fdump-rtl-bypass

    Dump after jump bypassing and control flow optimizations.

  .. option:: -fdump-rtl-combine

    Dump after the RTL instruction combination pass.

  .. option:: -fdump-rtl-compgotos

    Dump after duplicating the computed gotos.

  .. option:: -fdump-rtl-ce1, -fdump-rtl-ce2, -fdump-rtl-ce3

    :option:`-fdump-rtl-ce1`, :option:`-fdump-rtl-ce2`, and
    :option:`-fdump-rtl-ce3` enable dumping after the three
    if conversion passes.

  .. option:: -fdump-rtl-cprop_hardreg

    Dump after hard register copy propagation.

  .. option:: -fdump-rtl-csa

    Dump after combining stack adjustments.

  .. option:: -fdump-rtl-cse1, -fdump-rtl-cse2

    :option:`-fdump-rtl-cse1` and :option:`-fdump-rtl-cse2` enable dumping after
    the two common subexpression elimination passes.

  .. option:: -fdump-rtl-dce

    Dump after the standalone dead code elimination passes.

  .. option:: -fdump-rtl-dbr

    Dump after delayed branch scheduling.

  .. option:: -fdump-rtl-dce1, -fdump-rtl-dce2

    :option:`-fdump-rtl-dce1` and :option:`-fdump-rtl-dce2` enable dumping after
    the two dead store elimination passes.

  .. option:: -fdump-rtl-eh

    Dump after finalization of EH handling code.

  .. option:: -fdump-rtl-eh_ranges

    Dump after conversion of EH handling range regions.

  .. option:: -fdump-rtl-expand

    Dump after RTL generation.

  .. option:: -fdump-rtl-fwprop1, -fdump-rtl-fwprop2

    :option:`-fdump-rtl-fwprop1` and :option:`-fdump-rtl-fwprop2` enable
    dumping after the two forward propagation passes.

  .. option:: -fdump-rtl-gcse1, -fdump-rtl-gcse2

    :option:`-fdump-rtl-gcse1` and :option:`-fdump-rtl-gcse2` enable dumping
    after global common subexpression elimination.

  .. option:: -fdump-rtl-init-regs

    Dump after the initialization of the registers.

  .. option:: -fdump-rtl-initvals

    Dump after the computation of the initial value sets.

  .. option:: -fdump-rtl-into_cfglayout

    Dump after converting to cfglayout mode.

  .. option:: -fdump-rtl-ira

    Dump after iterated register allocation.

  .. option:: -fdump-rtl-jump

    Dump after the second jump optimization.

  .. option:: -fdump-rtl-loop2

    :option:`-fdump-rtl-loop2` enables dumping after the rtl
    loop optimization passes.

  .. option:: -fdump-rtl-mach

    Dump after performing the machine dependent reorganization pass, if that
    pass exists.

  .. option:: -fdump-rtl-mode_sw

    Dump after removing redundant mode switches.

  .. option:: -fdump-rtl-rnreg

    Dump after register renumbering.

  .. option:: -fdump-rtl-outof_cfglayout

    Dump after converting from cfglayout mode.

  .. option:: -fdump-rtl-peephole2

    Dump after the peephole pass.

  .. option:: -fdump-rtl-postreload

    Dump after post-reload optimizations.

  .. option:: -fdump-rtl-pro_and_epilogue

    Dump after generating the function prologues and epilogues.

  .. option:: -fdump-rtl-sched1, -fdump-rtl-sched2

    :option:`-fdump-rtl-sched1` and :option:`-fdump-rtl-sched2` enable dumping
    after the basic block scheduling passes.

  .. option:: -fdump-rtl-ree

    Dump after sign/zero extension elimination.

  .. option:: -fdump-rtl-seqabstr

    Dump after common sequence discovery.

  .. option:: -fdump-rtl-shorten

    Dump after shortening branches.

  .. option:: -fdump-rtl-sibling

    Dump after sibling call optimizations.

  .. option:: -fdump-rtl-split1, -fdump-rtl-split2, -fdump-rtl-split3, -fdump-rtl-split4, -fdump-rtl-split5

    These options enable dumping after five rounds of
    instruction splitting.

  .. option:: -fdump-rtl-sms

    Dump after modulo scheduling.  This pass is only run on some
    architectures.

  .. option:: -fdump-rtl-stack

    Dump after conversion from GCC's 'flat register file' registers to the
    x87's stack-like registers.  This pass is only run on x86 variants.

  .. option:: -fdump-rtl-subreg1, -fdump-rtl-subreg2

    :option:`-fdump-rtl-subreg1` and :option:`-fdump-rtl-subreg2` enable dumping after
    the two subreg expansion passes.

  .. option:: -fdump-rtl-unshare

    Dump after all rtl has been unshared.

  .. option:: -fdump-rtl-vartrack

    Dump after variable tracking.

  .. option:: -fdump-rtl-vregs

    Dump after converting virtual registers to hard registers.

  .. option:: -fdump-rtl-web

    Dump after live range splitting.

  .. option:: -fdump-rtl-regclass, -fdump-rtl-subregs_of_mode_init, -fdump-rtl-subregs_of_mode_finish, -fdump-rtl-dfinit, -fdump-rtl-dfinish

    These dumps are defined but always produce empty files.

  .. option:: -da, -fdump-rtl-all

    Produce all the dumps listed above.

  .. option:: -dA

    Annotate the assembler output with miscellaneous debugging information.

  .. option:: -dD

    Dump all macro definitions, at the end of preprocessing, in addition to
    normal output.

  .. option:: -dH

    Produce a core dump whenever an error occurs.

  .. option:: -dp

    Annotate the assembler output with a comment indicating which
    pattern and alternative is used.  The length and cost of each instruction are
    also printed.

  .. option:: -dP

    Dump the RTL in the assembler output as a comment before each instruction.
    Also turns on :option:`-dp` annotation.

  .. option:: -dx

    Just generate RTL for a function instead of compiling it.  Usually used
    with :option:`-fdump-rtl-expand`.

.. option:: -fdump-debug

  Dump debugging information generated during the debug
  generation phase.

.. option:: -fdump-earlydebug

  Dump debugging information generated during the early debug
  generation phase.

.. option:: -fdump-noaddr

  When doing debugging dumps, suppress address output.  This makes it more
  feasible to use diff on debugging dumps for compiler invocations with
  different compiler binaries and/or different
  text / bss / data / heap / stack / dso start locations.

.. option:: -freport-bug

  Collect and dump debug information into a temporary file if an
  internal compiler error (ICE) occurs.

.. option:: -fdump-unnumbered

  When doing debugging dumps, suppress instruction numbers and address output.
  This makes it more feasible to use diff on debugging dumps for compiler
  invocations with different options, in particular with and without
  :option:`-g`.

.. option:: -fdump-unnumbered-links

  When doing debugging dumps (see :option:`-d` option above), suppress
  instruction numbers for the links to the previous and next instructions
  in a sequence.

.. option:: -fdump-ipa-switch, -fdump-ipa-switch-options

  Control the dumping at various stages of inter-procedural analysis
  language tree to a file.  The file name is generated by appending a
  switch specific suffix to the source file name, and the file is created
  in the same directory as the output file.  The following dumps are
  possible:

  :samp:`all`
    Enables all inter-procedural analysis dumps.

  :samp:`cgraph`
    Dumps information about call-graph optimization, unused function removal,
    and inlining decisions.

  :samp:`inline`
    Dump after function inlining.

  Additionally, the options :option:`-optimized`, :option:`-missed`,
  :option:`-note`, and :option:`-all` can be provided, with the same meaning
  as for :option:`-fopt-info`, defaulting to :option:`-optimized`.

  For example, :option:`-fdump-ipa-inline-optimized-missed` will emit
  information on callsites that were inlined, along with callsites
  that were not inlined.

  By default, the dump will contain messages about successful
  optimizations (equivalent to :option:`-optimized`) together with
  low-level details about the analysis.

.. option:: -fdump-lang

  Dump language-specific information.  The file name is made by appending
  :samp:`.lang` to the source file name.

.. option:: -fdump-lang-all, -fdump-lang-switch, -fdump-lang-switch-options, -fdump-lang-switch-options={filename}

  Control the dumping of language-specific information.  The :samp:`{options}`
  and :samp:`{filename}` portions behave as described in the
  :option:`-fdump-tree` option.  The following :samp:`{switch}` values are
  accepted:

  :samp:`all`
    Enable all language-specific dumps.

  :samp:`class`
    Dump class hierarchy information.  Virtual table information is emitted
    unless ' slim ' is specified.  This option is applicable to C++ only.

  :samp:`module`
    Dump module information.  Options lineno (locations),
    graph (reachability), blocks (clusters),
    uid (serialization), alias (mergeable),
    asmname (Elrond), eh (mapper) & vops
    (macros) may provide additional information.  This option is
    applicable to C++ only.

  :samp:`raw`
    Dump the raw internal tree data.  This option is applicable to C++ only.

.. option:: -fdump-passes

  Print on :samp:`stderr` the list of optimization passes that are turned
  on and off by the current command-line options.

.. option:: -fdump-statistics-option

  Enable and control dumping of pass statistics in a separate file.  The
  file name is generated by appending a suffix ending in
  :samp:`.statistics` to the source file name, and the file is created in
  the same directory as the output file.  If the :samp:`-{option}`
  form is used, :samp:`-stats` causes counters to be summed over the
  whole compilation unit while :samp:`-details` dumps every event as
  the passes generate them.  The default with no option is to sum
  counters for each function compiled.

.. option:: -fdump-tree-all, -fdump-tree-switch, -fdump-tree-switch-options, -fdump-tree-switch-options={filename}

  Control the dumping at various stages of processing the intermediate
  language tree to a file.  If the :samp:`-{options}`
  form is used, :samp:`{options}` is a list of :samp:`-` separated options
  which control the details of the dump.  Not all options are applicable
  to all dumps; those that are not meaningful are ignored.  The
  following options are available

  :samp:`address`
    Print the address of each node.  Usually this is not meaningful as it
    changes according to the environment and source file.  Its primary use
    is for tying up a dump file with a debug environment.

  :samp:`asmname`
    If ``DECL_ASSEMBLER_NAME`` has been set for a given decl, use that
    in the dump instead of ``DECL_NAME``.  Its primary use is ease of
    use working backward from mangled names in the assembly file.

  :samp:`slim`
    When dumping front-end intermediate representations, inhibit dumping
    of members of a scope or body of a function merely because that scope
    has been reached.  Only dump such items when they are directly reachable
    by some other path.

    When dumping pretty-printed trees, this option inhibits dumping the
    bodies of control structures.

    When dumping RTL, print the RTL in slim (condensed) form instead of
    the default LISP-like representation.

  :samp:`raw`
    Print a raw representation of the tree.  By default, trees are
    pretty-printed into a C-like representation.

  :samp:`details`
    Enable more detailed dumps (not honored by every dump option). Also
    include information from the optimization passes.

  :samp:`stats`
    Enable dumping various statistics about the pass (not honored by every dump
    option).

  :samp:`blocks`
    Enable showing basic block boundaries (disabled in raw dumps).

  :samp:`graph`
    For each of the other indicated dump files (:option:`-fdump-rtl-pass`),
    dump a representation of the control flow graph suitable for viewing with
    GraphViz to :samp:`{file}.{passid}.{pass}.dot`.  Each function in
    the file is pretty-printed as a subgraph, so that GraphViz can render them
    all in a single plot.

    This option currently only works for RTL dumps, and the RTL is always
    dumped in slim form.

  :samp:`vops`
    Enable showing virtual operands for every statement.

  :samp:`lineno`
    Enable showing line numbers for statements.

  :samp:`uid`
    Enable showing the unique ID (``DECL_UID``) for each variable.

  :samp:`verbose`
    Enable showing the tree dump for each statement.

  :samp:`eh`
    Enable showing the EH region number holding each statement.

  :samp:`scev`
    Enable showing scalar evolution analysis details.

  :samp:`optimized`
    Enable showing optimization information (only available in certain
    passes).

  :samp:`missed`
    Enable showing missed optimization information (only available in certain
    passes).

  :samp:`note`
    Enable other detailed optimization information (only available in
    certain passes).

  :samp:`all`
    Turn on all options, except raw, slim, verbose
    and lineno.

  :samp:`optall`
    Turn on all optimization options, i.e., optimized,
    missed, and note.

  To determine what tree dumps are available or find the dump for a pass
  of interest follow the steps below.

  * Invoke GCC with :option:`-fdump-passes` and in the :samp:`stderr` output
    look for a code that corresponds to the pass you are interested in.
    For example, the codes ``tree-evrp``, ``tree-vrp1``, and
    ``tree-vrp2`` correspond to the three Value Range Propagation passes.
    The number at the end distinguishes distinct invocations of the same pass.

  * To enable the creation of the dump file, append the pass code to
    the :option:`-fdump-` option prefix and invoke GCC with it.  For example,
    to enable the dump from the Early Value Range Propagation pass, invoke
    GCC with the :option:`-fdump-tree-evrp` option.  Optionally, you may
    specify the name of the dump file.  If you don't specify one, GCC
    creates as described below.

  * Find the pass dump in a file whose name is composed of three components
    separated by a period: the name of the source file GCC was invoked to
    compile, a numeric suffix indicating the pass number followed by the
    letter :samp:`t` for tree passes (and the letter :samp:`r` for RTL passes),
    and finally the pass code.  For example, the Early VRP pass dump might
    be in a file named :samp:`myfile.c.038t.evrp` in the current working
    directory.  Note that the numeric codes are not stable and may change
    from one version of GCC to another.

.. option:: -fopt-info, -fopt-info-options, -fopt-info-options={filename}

  Controls optimization dumps from various optimization passes. If the
  :samp:`-{options}` form is used, :samp:`{options}` is a list of
  :samp:`-` separated option keywords to select the dump details and
  optimizations.

  The :samp:`{options}` can be divided into three groups:

  * options describing what kinds of messages should be emitted,

  * options describing the verbosity of the dump, and

  * options describing which optimizations should be included.

  The options from each group can be freely mixed as they are
  non-overlapping. However, in case of any conflicts,
  the later options override the earlier options on the command
  line.

  The following options control which kinds of messages should be emitted:

  :samp:`optimized`
    Print information when an optimization is successfully applied. It is
    up to a pass to decide which information is relevant. For example, the
    vectorizer passes print the source location of loops which are
    successfully vectorized.

  :samp:`missed`
    Print information about missed optimizations. Individual passes
    control which information to include in the output.

  :samp:`note`
    Print verbose information about optimizations, such as certain
    transformations, more detailed messages about decisions etc.

  :samp:`all`
    Print detailed optimization information. This includes
    :samp:`optimized`, :samp:`missed`, and :samp:`note`.

    The following option controls the dump verbosity:

  :samp:`internals`
    By default, only 'high-level' messages are emitted. This option enables
    additional, more detailed, messages, which are likely to only be of interest
    to GCC developers.

  One or more of the following option keywords can be used to describe a
  group of optimizations:

  :samp:`ipa`
    Enable dumps from all interprocedural optimizations.

  :samp:`loop`
    Enable dumps from all loop optimizations.

  :samp:`inline`
    Enable dumps from all inlining optimizations.

  :samp:`omp`
    Enable dumps from all OMP (Offloading and Multi Processing) optimizations.

  :samp:`vec`
    Enable dumps from all vectorization optimizations.

  :samp:`optall`
    Enable dumps from all optimizations. This is a superset of
    the optimization groups listed above.

  If :samp:`{options}` is
  omitted, it defaults to :samp:`optimized-optall`, which means to dump messages
  about successful optimizations from all the passes, omitting messages
  that are treated as 'internals'.

  If the :samp:`{filename}` is provided, then the dumps from all the
  applicable optimizations are concatenated into the :samp:`{filename}`.
  Otherwise the dump is output onto :samp:`stderr`. Though multiple
  :option:`-fopt-info` options are accepted, only one of them can include
  a :samp:`{filename}`. If other filenames are provided then all but the
  first such option are ignored.

  Note that the output :samp:`{filename}` is overwritten
  in case of multiple translation units. If a combined output from
  multiple translation units is desired, :samp:`stderr` should be used
  instead.

  In the following example, the optimization info is output to
  :samp:`stderr`:

  .. code-block:: shell

    gcc -O3 -fopt-info

  This example:

  .. code-block:: shell

    gcc -O3 -fopt-info-missed=missed.all

  outputs missed optimization report from all the passes into
  :samp:`missed.all`, and this one:

  .. code-block:: shell

    gcc -O2 -ftree-vectorize -fopt-info-vec-missed

  prints information about missed optimization opportunities from
  vectorization passes on :samp:`stderr`.
  Note that :option:`-fopt-info-vec-missed` is equivalent to
  :option:`-fopt-info-missed-vec`.  The order of the optimization group
  names and message types listed after :option:`-fopt-info` does not matter.

  As another example,

  .. code-block:: shell

    gcc -O3 -fopt-info-inline-optimized-missed=inline.txt

  outputs information about missed optimizations as well as
  optimized locations from all the inlining passes into
  :samp:`inline.txt`.

  Finally, consider:

  .. code-block:: shell

    gcc -fopt-info-vec-missed=vec.miss -fopt-info-loop-optimized=loop.opt

  Here the two output filenames :samp:`vec.miss` and :samp:`loop.opt` are
  in conflict since only one output file is allowed. In this case, only
  the first option takes effect and the subsequent options are
  ignored. Thus only :samp:`vec.miss` is produced which contains
  dumps from the vectorizer about missed opportunities.

.. option:: -fsave-optimization-record

  Write a SRCFILE.opt-record.json.gz file detailing what optimizations
  were performed, for those optimizations that support :option:`-fopt-info`.

  This option is experimental and the format of the data within the
  compressed JSON file is subject to change.

  It is roughly equivalent to a machine-readable version of
  :option:`-fopt-info-all`, as a collection of messages with source file,
  line number and column number, with the following additional data for
  each message:

  * the execution count of the code being optimized, along with metadata about
    whether this was from actual profile data, or just an estimate, allowing
    consumers to prioritize messages by code hotness,

  * the function name of the code being optimized, where applicable,

  * the 'inlining chain' for the code being optimized, so that when
    a function is inlined into several different places (which might
    themselves be inlined), the reader can distinguish between the copies,

  * objects identifying those parts of the message that refer to expressions,
    statements or symbol-table nodes, which of these categories they are, and,
    when available, their source code location,

  * the GCC pass that emitted the message, and

  * the location in GCC's own code from which the message was emitted

  Additionally, some messages are logically nested within other
  messages, reflecting implementation details of the optimization
  passes.

.. option:: -fsched-verbose={n}

  On targets that use instruction scheduling, this option controls the
  amount of debugging output the scheduler prints to the dump files.

  For :samp:`{n}` greater than zero, :option:`-fsched-verbose` outputs the
  same information as :option:`-fdump-rtl-sched1` and :option:`-fdump-rtl-sched2`.
  For :samp:`{n}` greater than one, it also output basic block probabilities,
  detailed ready list information and unit/insn info.  For :samp:`{n}` greater
  than two, it includes RTL at abort point, control-flow and regions info.
  And for :samp:`{n}` over four, :option:`-fsched-verbose` also includes
  dependence info.

.. option:: -fenable-kind-pass, -fdisable-kind-pass={range-list}

  This is a set of options that are used to explicitly disable/enable
  optimization passes.  These options are intended for use for debugging GCC.
  Compiler users should use regular options for enabling/disabling
  passes instead.

  :samp:`-fdisable-ipa-{pass}`
    Disable IPA pass :samp:`{pass}`. :samp:`{pass}` is the pass name.  If the same pass is
    statically invoked in the compiler multiple times, the pass name should be
    appended with a sequential number starting from 1.

  :samp:`-fdisable-rtl-{pass}` :samp:`-fdisable-rtl-{pass}={range-list}`
    Disable RTL pass :samp:`{pass}`.  :samp:`{pass}` is the pass name.  If the same pass is
    statically invoked in the compiler multiple times, the pass name should be
    appended with a sequential number starting from 1.  :samp:`{range-list}` is a
    comma-separated list of function ranges or assembler names.  Each range is a number
    pair separated by a colon.  The range is inclusive in both ends.  If the range
    is trivial, the number pair can be simplified as a single number.  If the
    function's call graph node's :samp:`{uid}` falls within one of the specified ranges,
    the :samp:`{pass}` is disabled for that function.  The :samp:`{uid}` is shown in the
    function header of a dump file, and the pass names can be dumped by using
    option :option:`-fdump-passes`.

  :samp:`-fdisable-tree-{pass}` :samp:`-fdisable-tree-{pass}={range-list}`
    Disable tree pass :samp:`{pass}`.  See :option:`-fdisable-rtl` for the description of
    option arguments.

  :samp:`-fenable-ipa-{pass}`
    Enable IPA pass :samp:`{pass}`.  :samp:`{pass}` is the pass name.  If the same pass is
    statically invoked in the compiler multiple times, the pass name should be
    appended with a sequential number starting from 1.

  :samp:`-fenable-rtl-{pass}` :samp:`-fenable-rtl-{pass}={range-list}`
    Enable RTL pass :samp:`{pass}`.  See :option:`-fdisable-rtl` for option argument
    description and examples.

  :samp:`-fenable-tree-{pass}` :samp:`-fenable-tree-{pass}={range-list}`
    Enable tree pass :samp:`{pass}`.  See :option:`-fdisable-rtl` for the description
    of option arguments.

    Here are some examples showing uses of these options.

  .. code-block:: c++

    # disable ccp1 for all functions
       -fdisable-tree-ccp1
    # disable complete unroll for function whose cgraph node uid is 1
       -fenable-tree-cunroll=1
    # disable gcse2 for functions at the following ranges [1,1],
    # [300,400], and [400,1000]
    # disable gcse2 for functions foo and foo2
       -fdisable-rtl-gcse2=foo,foo2
    # disable early inlining
       -fdisable-tree-einline
    # disable ipa inlining
       -fdisable-ipa-inline
    # enable tree full unroll
       -fenable-tree-unroll

.. option:: -fchecking, -fchecking={n}

  Enable internal consistency checking.  The default depends on
  the compiler configuration.  :option:`-fchecking=2` enables further
  internal consistency checking that might affect code generation.

.. option:: -fno-checking

  Default setting; overrides :option:`-fchecking`.

.. option:: -frandom-seed={string}

  This option provides a seed that GCC uses in place of
  random numbers in generating certain symbol names
  that have to be different in every compiled file.  It is also used to
  place unique stamps in coverage data files and the object files that
  produce them.  You can use the :option:`-frandom-seed` option to produce
  reproducibly identical object files.

  The :samp:`{string}` can either be a number (decimal, octal or hex) or an
  arbitrary string (in which case it's converted to a number by
  computing CRC32).

  The :samp:`{string}` should be different for every file you compile.

.. option:: -save-temps

  Store the usual 'temporary' intermediate files permanently; name them
  as auxiliary output files, as specified described under
  :option:`-dumpbase` and :option:`-dumpdir`.

  When used in combination with the :option:`-x` command-line option,
  :option:`-save-temps` is sensible enough to avoid overwriting an
  input source file with the same extension as an intermediate file.
  The corresponding intermediate file may be obtained by renaming the
  source file before using :option:`-save-temps`.

.. option:: -save-temps=cwd

  Equivalent to :option:`-save-temps -dumpdir ./`.

.. option:: -save-temps=obj

  Equivalent to :option:`-save-temps -dumpdir outdir/`, where
  :samp:`outdir/` is the directory of the output file specified after the
  :option:`-o` option, including any directory separators.  If the
  :option:`-o` option is not used, the :option:`-save-temps=obj` switch
  behaves like :option:`-save-temps=cwd`.

.. option:: -time[={file}]

  Report the CPU time taken by each subprocess in the compilation
  sequence.  For C source files, this is the compiler proper and assembler
  (plus the linker if linking is done).

  Without the specification of an output file, the output looks like this:

  .. code-block:: c++

    # cc1 0.12 0.01
    # as 0.00 0.01

  The first number on each line is the 'user time', that is time spent
  executing the program itself.  The second number is 'system time',
  time spent executing operating system routines on behalf of the program.
  Both numbers are in seconds.

  With the specification of an output file, the output is appended to the
  named file, and it looks like this:

  .. code-block:: c++

    0.12 0.01 cc1 options
    0.00 0.01 as options

  The 'user time' and the 'system time' are moved before the program
  name, and the options passed to the program are displayed, so that one
  can later tell what file was being compiled, and with which options.

.. option:: -fdump-final-insns[={file}]

  Dump the final internal representation (RTL) to :samp:`{file}`.  If the
  optional argument is omitted (or if :samp:`{file}` is ``.``), the name
  of the dump file is determined by appending ``.gkd`` to the
  dump base name, see :option:`-dumpbase`.

.. option:: -fcompare-debug[={opts}]

  If no error occurs during compilation, run the compiler a second time,
  adding :samp:`{opts}` and :option:`-fcompare-debug-second` to the arguments
  passed to the second compilation.  Dump the final internal
  representation in both compilations, and print an error if they differ.

  If the equal sign is omitted, the default :option:`-gtoggle` is used.

  The environment variable :envvar:`GCC_COMPARE_DEBUG`, if defined, non-empty
  and nonzero, implicitly enables :option:`-fcompare-debug`.  If
  :envvar:`GCC_COMPARE_DEBUG` is defined to a string starting with a dash,
  then it is used for :samp:`{opts}`, otherwise the default :option:`-gtoggle`
  is used.

  :option:`-fcompare-debug=`, with the equal sign but without :samp:`{opts}`,
  is equivalent to :option:`-fno-compare-debug`, which disables the dumping
  of the final representation and the second compilation, preventing even
  :envvar:`GCC_COMPARE_DEBUG` from taking effect.

  To verify full coverage during :option:`-fcompare-debug` testing, set
  :envvar:`GCC_COMPARE_DEBUG` to say :option:`-fcompare-debug-not-overridden`,
  which GCC rejects as an invalid option in any actual compilation
  (rather than preprocessing, assembly or linking).  To get just a
  warning, setting :envvar:`GCC_COMPARE_DEBUG` to :samp:`-w%n-fcompare-debug
  not overridden` will do.

.. option:: -fcompare-debug-second

  This option is implicitly passed to the compiler for the second
  compilation requested by :option:`-fcompare-debug`, along with options to
  silence warnings, and omitting other options that would cause the compiler
  to produce output to files or to standard output as a side effect.  Dump
  files and preserved temporary files are renamed so as to contain the
  ``.gk`` additional extension during the second compilation, to avoid
  overwriting those generated by the first.

  When this option is passed to the compiler driver, it causes the
  *first* compilation to be skipped, which makes it useful for little
  other than debugging the compiler proper.

.. option:: -gtoggle

  Turn off generation of debug info, if leaving out this option
  generates it, or turn it on at level 2 otherwise.  The position of this
  argument in the command line does not matter; it takes effect after all
  other options are processed, and it does so only once, no matter how
  many times it is given.  This is mainly intended to be used with
  :option:`-fcompare-debug`.

.. option:: -fvar-tracking-assignments-toggle

  Toggle :option:`-fvar-tracking-assignments`, in the same way that
  :option:`-gtoggle` toggles :option:`-g`.

.. option:: -fno-var-tracking-assignments-toggle

  Default setting; overrides :option:`-fvar-tracking-assignments-toggle`.

.. option:: -Q

  Makes the compiler print out each function name as it is compiled, and
  print some statistics about each pass when it finishes.

.. option:: -ftime-report

  Makes the compiler print some statistics about the time consumed by each
  pass when it finishes.

.. option:: -ftime-report-details

  Record the time consumed by infrastructure parts separately for each pass.

.. option:: -fira-verbose={n}

  Control the verbosity of the dump file for the integrated register allocator.
  The default value is 5.  If the value :samp:`{n}` is greater or equal to 10,
  the dump output is sent to stderr using the same format as :samp:`{n}` minus 10.

.. option:: -flto-report

  Prints a report with internal details on the workings of the link-time
  optimizer.  The contents of this report vary from version to version.
  It is meant to be useful to GCC developers when processing object
  files in LTO mode (via :option:`-flto`).

  Disabled by default.

.. option:: -flto-report-wpa

  Like :option:`-flto-report`, but only print for the WPA phase of link-time
  optimization.

.. option:: -fmem-report

  Makes the compiler print some statistics about permanent memory
  allocation when it finishes.

.. option:: -fmem-report-wpa

  Makes the compiler print some statistics about permanent memory
  allocation for the WPA phase only.

.. option:: -fpre-ipa-mem-report

.. option:: -fpost-ipa-mem-report

  Makes the compiler print some statistics about permanent memory
  allocation before or after interprocedural optimization.

.. option:: -fmultiflags

  This option enables multilib-aware ``TFLAGS`` to be used to build
  target libraries with options different from those the compiler is
  configured to use by default, through the use of specs (See :ref:`spec-files`) set up by compiler internals, by the target, or by builders at
  configure time.

  Like ``TFLAGS``, this allows the target libraries to be built for
  portable baseline environments, while the compiler defaults to more
  demanding ones.  That's useful because users can easily override the
  defaults the compiler is configured to use to build their own programs,
  if the defaults are not ideal for their target environment, whereas
  rebuilding the runtime libraries is usually not as easy or desirable.

  Unlike ``TFLAGS``, the use of specs enables different flags to be
  selected for different multilibs.  The way to accomplish that is to
  build with :samp:`make TFLAGS=-fmultiflags`, after configuring
  :samp:`--with-specs=%{fmultiflags:...}`.

  This option is discarded by the driver once it's done processing driver
  self spec.

  It is also useful to check that ``TFLAGS`` are being used to build
  all target libraries, by configuring a non-bootstrap compiler
  :samp:`--with-specs='%{!fmultiflags:%emissing TFLAGS}'` and building
  the compiler and target libraries.

.. option:: -fprofile-report

  Makes the compiler print some statistics about consistency of the
  (estimated) profile and effect of individual passes.

.. option:: -fstack-usage

  Makes the compiler output stack usage information for the program, on a
  per-function basis.  The filename for the dump is made by appending
  :samp:`.su` to the :samp:`{auxname}`.  :samp:`{auxname}` is generated from the name of
  the output file, if explicitly specified and it is not an executable,
  otherwise it is the basename of the source file.  An entry is made up
  of three fields:

  * The name of the function.

  * A number of bytes.

  * One or more qualifiers: ``static``, ``dynamic``, ``bounded``.

  The qualifier ``static`` means that the function manipulates the stack
  statically: a fixed number of bytes are allocated for the frame on function
  entry and released on function exit; no stack adjustments are otherwise made
  in the function.  The second field is this fixed number of bytes.

  The qualifier ``dynamic`` means that the function manipulates the stack
  dynamically: in addition to the static allocation described above, stack
  adjustments are made in the body of the function, for example to push/pop
  arguments around function calls.  If the qualifier ``bounded`` is also
  present, the amount of these adjustments is bounded at compile time and
  the second field is an upper bound of the total amount of stack used by
  the function.  If it is not present, the amount of these adjustments is
  not bounded at compile time and the second field only represents the
  bounded part.

.. option:: -fstats

  Emit statistics about front-end processing at the end of the compilation.
  This option is supported only by the C++ front end, and
  the information is generally only useful to the G++ development team.

.. option:: -fdbg-cnt-list

  Print the name and the counter upper bound for all debug counters.

.. option:: -fdbg-cnt={counter-value-list}

  Set the internal debug counter lower and upper bound.  :samp:`{counter-value-list}`
  is a comma-separated list of :samp:`{name}:{lower_bound1}-{upper_bound1}`
  :samp:`[:{lower_bound2}-{upper_bound2}...]` tuples which sets
  the name of the counter and list of closed intervals.
  The :samp:`{lower_bound}` is optional and is zero
  initialized if not set.
  For example, with :option:`-fdbg-cnt=dce:2-4:10-11,tail_call:10`,
  ``dbg_cnt(dce)`` returns true only for second, third, fourth, tenth and
  eleventh invocation.
  For ``dbg_cnt(tail_call)`` true is returned for first 10 invocations.

.. option:: -print-file-name={library}

  Print the full absolute name of the library file :samp:`{library}` that
  would be used when linking---and don't do anything else.  With this
  option, GCC does not compile or link anything; it just prints the
  file name.

.. option:: -print-multi-directory

  Print the directory name corresponding to the multilib selected by any
  other switches present in the command line.  This directory is supposed
  to exist in :envvar:`GCC_EXEC_PREFIX`.

.. option:: -print-multi-lib

  Print the mapping from multilib directory names to compiler switches
  that enable them.  The directory name is separated from the switches by
  :samp:`;`, and each switch starts with an :samp:`@` instead of the
  :samp:`-`, without spaces between multiple switches.  This is supposed to
  ease shell processing.

.. option:: -print-multi-os-directory

  Print the path to OS libraries for the selected
  multilib, relative to some :samp:`lib` subdirectory.  If OS libraries are
  present in the :samp:`lib` subdirectory and no multilibs are used, this is
  usually just :samp:`.`, if OS libraries are present in :samp:`lib{suffix}`
  sibling directories this prints e.g. :samp:`../lib64`, :samp:`../lib` or
  :samp:`../lib32`, or if OS libraries are present in :samp:`lib/{subdir}`
  subdirectories it prints e.g. :samp:`amd64`, :samp:`sparcv9` or :samp:`ev6`.

.. option:: -print-multiarch

  Print the path to OS libraries for the selected multiarch,
  relative to some :samp:`lib` subdirectory.

.. option:: -print-prog-name={program}

  Like :option:`-print-file-name`, but searches for a program such as :command:`cpp`.

.. option:: -print-libgcc-file-name

  Same as :option:`-print-file-name=libgcc.a`.

  This is useful when you use :option:`-nostdlib` or :option:`-nodefaultlibs`
  but you do want to link with :samp:`libgcc.a`.  You can do:

  .. code-block:: shell

    gcc -nostdlib files... `gcc -print-libgcc-file-name`

.. option:: -print-search-dirs

  Print the name of the configured installation directory and a list of
  program and library directories :command:`gcc` searches---and don't do anything else.

  This is useful when :command:`gcc` prints the error message
  :samp:`installation problem, cannot exec cpp0: No such file or directory`.
  To resolve this you either need to put :samp:`cpp0` and the other compiler
  components where :command:`gcc` expects to find them, or you can set the environment
  variable :envvar:`GCC_EXEC_PREFIX` to the directory where you installed them.
  Don't forget the trailing :samp:`/`.
  See :ref:`environment-variables`.

.. option:: -print-sysroot

  Print the target sysroot directory that is used during
  compilation.  This is the target sysroot specified either at configure
  time or using the :option:`--sysroot` option, possibly with an extra
  suffix that depends on compilation options.  If no target sysroot is
  specified, the option prints nothing.

.. option:: -print-sysroot-headers-suffix

  Print the suffix added to the target sysroot when searching for
  headers, or give an error if the compiler is not configured with such
  a suffix---and don't do anything else.

.. option:: -dumpmachine

  Print the compiler's target machine (for example,
  :samp:`i686-pc-linux-gnu`)---and don't do anything else.

.. option:: -dumpversion

  Print the compiler version (for example, ``3.0``, ``6.3.0`` or ``7``)---and don't do
  anything else.  This is the compiler version used in filesystem paths and
  specs. Depending on how the compiler has been configured it can be just
  a single number (major version), two numbers separated by a dot (major and
  minor version) or three numbers separated by dots (major, minor and patchlevel
  version).

.. option:: -dumpfullversion

  Print the full compiler version---and don't do anything else. The output is
  always three numbers separated by dots, major, minor and patchlevel version.

.. option:: -dumpspecs

  Print the compiler's built-in specs---and don't do anything else.  (This
  is used when GCC itself is being built.)  See :ref:`spec-files`.
