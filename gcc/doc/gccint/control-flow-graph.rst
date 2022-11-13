..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: CFG, Control Flow Graph, basic-block.h

.. _control-flow:

Control Flow Graph
------------------

A control flow graph (CFG) is a data structure built on top of the
intermediate code representation (the RTL or ``GIMPLE`` instruction
stream) abstracting the control flow behavior of a function that is
being compiled.  The CFG is a directed graph where the vertices
represent basic blocks and edges represent possible transfer of
control flow from one basic block to another.  The data structures
used to represent the control flow graph are defined in
:samp:`basic-block.h`.

In GCC, the representation of control flow is maintained throughout
the compilation process, from constructing the CFG early in
``pass_build_cfg`` to ``pass_free_cfg`` (see :samp:`passes.def`).
The CFG takes various different modes and may undergo extensive
manipulations, but the graph is always valid between its construction
and its release.  This way, transfer of information such as data flow,
a measured profile, or the loop tree, can be propagated through the
passes pipeline, and even from ``GIMPLE`` to ``RTL``.

Often the CFG may be better viewed as integral part of instruction
chain, than structure built on the top of it.  Updating the compiler's
intermediate representation for instructions cannot be easily done
without proper maintenance of the CFG simultaneously.

.. toctree::
  :maxdepth: 2

  control-flow-graph/basic-blocks
  control-flow-graph/edges
  control-flow-graph/profile-information
  control-flow-graph/maintaining-the-cfg
  control-flow-graph/liveness-information