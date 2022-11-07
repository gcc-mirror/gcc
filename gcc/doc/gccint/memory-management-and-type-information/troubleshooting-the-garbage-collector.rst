..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: garbage collector, troubleshooting

.. _troubleshooting:

Troubleshooting the garbage collector
*************************************

With the current garbage collector implementation, most issues should
show up as GCC compilation errors.  Some of the most commonly
encountered issues are described below.

* Gengtype does not produce allocators for a ``GTY`` -marked type.
  Gengtype checks if there is at least one possible path from GC roots to
  at least one instance of each type before outputting allocators.  If
  there is no such path, the ``GTY`` markers will be ignored and no
  allocators will be output.  Solve this by making sure that there exists
  at least one such path.  If creating it is unfeasible or raises a 'code
  smell', consider if you really must use GC for allocating such type.

* Link-time errors about undefined ``gt_ggc_r_foo_bar`` and
  similarly-named symbols.  Check if your :samp:`foo_bar` source file has
  ``#include "gt-foo_bar.h"`` as its very last line.