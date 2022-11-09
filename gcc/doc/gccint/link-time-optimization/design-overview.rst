..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _lto-overview:

Design Overview
***************

Link time optimization is implemented as a GCC front end for a
bytecode representation of GIMPLE that is emitted in special sections
of ``.o`` files.  Currently, LTO support is enabled in most
ELF-based systems, as well as darwin, cygwin and mingw systems.

By default, object files generated with LTO support contain only GIMPLE
bytecode.  Such objects are called 'slim', and they require that
tools like ``ar`` and ``nm`` understand symbol tables of LTO
sections.  For most targets these tools have been extended to use the
plugin infrastructure, so GCC can support 'slim' objects consisting
of the intermediate code alone.

GIMPLE bytecode could also be saved alongside final object code if
the :option:`-ffat-lto-objects` option is passed, or if no plugin support
is detected for ``ar`` and ``nm`` when GCC is configured.  It makes
the object files generated with LTO support larger than regular object
files.  This 'fat' object format allows to ship one set of fat
objects which could be used both for development and the production of
optimized builds.  A, perhaps surprising, side effect of this feature
is that any mistake in the toolchain leads to LTO information not
being used (e.g. an older ``libtool`` calling ``ld`` directly).
This is both an advantage, as the system is more robust, and a
disadvantage, as the user is not informed that the optimization has
been disabled.

At the highest level, LTO splits the compiler in two.  The first half
(the 'writer') produces a streaming representation of all the
internal data structures needed to optimize and generate code.  This
includes declarations, types, the callgraph and the GIMPLE representation
of function bodies.

When :option:`-flto` is given during compilation of a source file, the
pass manager executes all the passes in ``all_lto_gen_passes``.
Currently, this phase is composed of two IPA passes:

* ``pass_ipa_lto_gimple_out``
  This pass executes the function ``lto_output`` in
  :samp:`lto-streamer-out.cc`, which traverses the call graph encoding
  every reachable declaration, type and function.  This generates a
  memory representation of all the file sections described below.

* ``pass_ipa_lto_finish_out``
  This pass executes the function ``produce_asm_for_decls`` in
  :samp:`lto-streamer-out.cc`, which takes the memory image built in the
  previous pass and encodes it in the corresponding ELF file sections.

The second half of LTO support is the 'reader'.  This is implemented
as the GCC front end :samp:`lto1` in :samp:`lto/lto.cc`.  When
:samp:`collect2` detects a link set of ``.o`` / ``.a`` files with
LTO information and the :option:`-flto` is enabled, it invokes
:samp:`lto1` which reads the set of files and aggregates them into a
single translation unit for optimization.  The main entry point for
the reader is :samp:`lto/lto.cc`: ``lto_main``.

LTO modes of operation
^^^^^^^^^^^^^^^^^^^^^^

One of the main goals of the GCC link-time infrastructure was to allow
effective compilation of large programs.  For this reason GCC implements two
link-time compilation modes.

* *LTO mode*, in which the whole program is read into the
  compiler at link-time and optimized in a similar way as if it
  were a single source-level compilation unit.

* *WHOPR or partitioned mode*, designed to utilize multiple
  CPUs and/or a distributed compilation environment to quickly link
  large applications.  WHOPR stands for WHOle Program optimizeR (not to
  be confused with the semantics of :option:`-fwhole-program`).  It
  partitions the aggregated callgraph from many different ``.o``
  files and distributes the compilation of the sub-graphs to different
  CPUs.

  Note that distributed compilation is not implemented yet, but since
  the parallelism is facilitated via generating a ``Makefile``, it
  would be easy to implement.

WHOPR splits LTO into three main stages:

* Local generation (LGEN)
  This stage executes in parallel.  Every file in the program is compiled
  into the intermediate language and packaged together with the local
  call-graph and summary information.  This stage is the same for both
  the LTO and WHOPR compilation mode.

* Whole Program Analysis (WPA)
  WPA is performed sequentially.  The global call-graph is generated, and
  a global analysis procedure makes transformation decisions.  The global
  call-graph is partitioned to facilitate parallel optimization during
  phase 3.  The results of the WPA stage are stored into new object files
  which contain the partitions of program expressed in the intermediate
  language and the optimization decisions.

* Local transformations (LTRANS)
  This stage executes in parallel.  All the decisions made during phase 2
  are implemented locally in each partitioned object file, and the final
  object code is generated.  Optimizations which cannot be decided
  efficiently during the phase 2 may be performed on the local
  call-graph partitions.

WHOPR can be seen as an extension of the usual LTO mode of
compilation.  In LTO, WPA and LTRANS are executed within a single
execution of the compiler, after the whole program has been read into
memory.

When compiling in WHOPR mode, the callgraph is partitioned during
the WPA stage.  The whole program is split into a given number of
partitions of roughly the same size.  The compiler tries to
minimize the number of references which cross partition boundaries.
The main advantage of WHOPR is to allow the parallel execution of
LTRANS stages, which are the most time-consuming part of the
compilation process.  Additionally, it avoids the need to load the
whole program into memory.
