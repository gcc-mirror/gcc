..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _loop-analysis-and-representation:

Analysis and Representation of Loops
------------------------------------

GCC provides extensive infrastructure for work with natural loops, i.e.,
strongly connected components of CFG with only one entry block.  This
chapter describes representation of loops in GCC, both on GIMPLE and in
RTL, as well as the interfaces to loop-related analyses (induction
variable analysis and number of iterations analysis).

.. toctree::
  :maxdepth: 2

  analysis-and-representation-of-loops/loop-representation
  analysis-and-representation-of-loops/loop-querying
  analysis-and-representation-of-loops/loop-manipulation
  analysis-and-representation-of-loops/loop-closed-ssa-form
  analysis-and-representation-of-loops/scalar-evolutions
  analysis-and-representation-of-loops/iv-analysis-on-rtl
  analysis-and-representation-of-loops/number-of-iterations-analysis
  analysis-and-representation-of-loops/data-dependency-analysis