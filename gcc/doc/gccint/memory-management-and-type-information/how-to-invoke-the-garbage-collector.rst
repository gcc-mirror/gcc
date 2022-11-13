..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: garbage collector, invocation, ggc_collect

.. _invoking-the-garbage-collector:

How to invoke the garbage collector
***********************************

The GCC garbage collector GGC is only invoked explicitly. In contrast
with many other garbage collectors, it is not implicitly invoked by
allocation routines when a lot of memory has been consumed. So the
only way to have GGC reclaim storage is to call the ``ggc_collect``
function explicitly.
With :samp:`{mode}` ``GGC_COLLECT_FORCE`` or otherwise (default
``GGC_COLLECT_HEURISTIC``) when the internal heuristic decides to
collect, this call is potentially an expensive operation, as it may
have to scan the entire heap.  Beware that local variables (on the GCC
call stack) are not followed by such an invocation (as many other
garbage collectors do): you should reference all your data from static
or external ``GTY`` -ed variables, and it is advised to call
``ggc_collect`` with a shallow call stack.  The GGC is an exact mark
and sweep garbage collector (so it does not scan the call stack for
pointers).  In practice GCC passes don't often call ``ggc_collect``
themselves, because it is called by the pass manager between passes.

At the time of the ``ggc_collect`` call all pointers in the GC-marked
structures must be valid or ``NULL``.  In practice this means that
there should not be uninitialized pointer fields in the structures even
if your code never reads or writes those fields at a particular
instance.  One way to ensure this is to use cleared versions of
allocators unless all the fields are initialized manually immediately
after allocation.