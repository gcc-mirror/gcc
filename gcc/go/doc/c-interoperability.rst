..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _c-interoperability:

C Interoperability
------------------

When using :command:`gccgo` there is limited interoperability with C,
or with C++ code compiled using ``extern "C"``.

This information is provided largely for documentation purposes.  For
ordinary use it is best to build programs with the go tool and then
use ``import "C"``, as described at
https://golang.org/cmd/cgo.

.. toctree::
  :maxdepth: 2

  c-type-interoperability
  function-names