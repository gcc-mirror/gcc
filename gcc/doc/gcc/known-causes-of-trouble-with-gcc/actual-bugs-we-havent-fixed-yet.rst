..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _actual-bugs:

Actual Bugs We Haven't Fixed Yet
********************************

* The ``fixincludes`` script interacts badly with automounters; if the
  directory of system header files is automounted, it tends to be
  unmounted while ``fixincludes`` is running.  This would seem to be a
  bug in the automounter.  We don't know any good way to work around it.
