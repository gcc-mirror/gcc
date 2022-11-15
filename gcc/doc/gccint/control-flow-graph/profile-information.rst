..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: profile representation

.. _profile-information:

Profile information
*******************

In many cases a compiler must make a choice whether to trade speed in
one part of code for speed in another, or to trade code size for code
speed.  In such cases it is useful to know information about how often
some given block will be executed.  That is the purpose for
maintaining profile within the flow graph.
GCC can handle profile information obtained through :dfn:`profile
feedback`, but it can also estimate branch probabilities based on
statics and heuristics.

.. index:: profile feedback

The feedback based profile is produced by compiling the program with
instrumentation, executing it on a train run and reading the numbers
of executions of basic blocks and edges back to the compiler while
re-compiling the program to produce the final executable.  This method
provides very accurate information about where a program spends most
of its time on the train run.  Whether it matches the average run of
course depends on the choice of train data set, but several studies
have shown that the behavior of a program usually changes just
marginally over different data sets.

.. index:: Static profile estimation, branch prediction, predict.def

When profile feedback is not available, the compiler may be asked to
attempt to predict the behavior of each branch in the program using a
set of heuristics (see :samp:`predict.def` for details) and compute
estimated frequencies of each basic block by propagating the
probabilities over the graph.

.. index:: frequency, count, BB_FREQ_BASE

Each ``basic_block`` contains two integer fields to represent
profile information: ``frequency`` and ``count``.  The
``frequency`` is an estimation how often is basic block executed
within a function.  It is represented as an integer scaled in the
range from 0 to ``BB_FREQ_BASE``.  The most frequently executed
basic block in function is initially set to ``BB_FREQ_BASE`` and
the rest of frequencies are scaled accordingly.  During optimization,
the frequency of the most frequent basic block can both decrease (for
instance by loop unrolling) or grow (for instance by cross-jumping
optimization), so scaling sometimes has to be performed multiple
times.

.. index:: gcov_type

The ``count`` contains hard-counted numbers of execution measured
during training runs and is nonzero only when profile feedback is
available.  This value is represented as the host's widest integer
(typically a 64 bit integer) of the special type ``gcov_type``.

Most optimization passes can use only the frequency information of a
basic block, but a few passes may want to know hard execution counts.
The frequencies should always match the counts after scaling, however
during updating of the profile information numerical error may
accumulate into quite large errors.

.. index:: REG_BR_PROB_BASE, EDGE_FREQUENCY

Each edge also contains a branch probability field: an integer in the
range from 0 to ``REG_BR_PROB_BASE``.  It represents probability of
passing control from the end of the ``src`` basic block to the
``dest`` basic block, i.e. the probability that control will flow
along this edge.  The ``EDGE_FREQUENCY`` macro is available to
compute how frequently a given edge is taken.  There is a ``count``
field for each edge as well, representing same information as for a
basic block.

The basic block frequencies are not represented in the instruction
stream, but in the RTL representation the edge frequencies are
represented for conditional jumps (via the ``REG_BR_PROB``
macro) since they are used when instructions are output to the
assembly file and the flow graph is no longer maintained.

.. index:: reverse probability

The probability that control flow arrives via a given edge to its
destination basic block is called :dfn:`reverse probability` and is not
directly represented, but it may be easily computed from frequencies
of basic blocks.

.. index:: redirect_edge_and_branch

Updating profile information is a delicate task that can unfortunately
not be easily integrated with the CFG manipulation API.  Many of the
functions and hooks to modify the CFG, such as
``redirect_edge_and_branch``, do not have enough information to
easily update the profile, so updating it is in the majority of cases
left up to the caller.  It is difficult to uncover bugs in the profile
updating code, because they manifest themselves only by producing
worse code, and checking profile consistency is not possible because
of numeric error accumulation.  Hence special attention needs to be
given to this issue in each pass that modifies the CFG.

.. index:: REG_BR_PROB_BASE, BB_FREQ_BASE, count

It is important to point out that ``REG_BR_PROB_BASE`` and
``BB_FREQ_BASE`` are both set low enough to be possible to compute
second power of any frequency or probability in the flow graph, it is
not possible to even square the ``count`` field, as modern CPUs are
fast enough to execute $2^32$ operations quickly.
