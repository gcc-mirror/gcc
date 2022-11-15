..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: host configuration

.. _host-config:

Host Configuration
------------------

Most details about the machine and system on which the compiler is
actually running are detected by the :command:`configure` script.  Some
things are impossible for :command:`configure` to detect; these are
described in two ways, either by macros defined in a file named
:samp:`xm-{machine}.h` or by hook functions in the file specified
by the :samp:`{out_host_hook_obj}` variable in :samp:`config.gcc`.  (The
intention is that very few hosts will need a header file but nearly
every fully supported host will need to override some hooks.)

If you need to define only a few macros, and they have simple
definitions, consider using the ``xm_defines`` variable in your
:samp:`config.gcc` entry instead of creating a host configuration
header.  See :ref:`system-config`.

.. toctree::
  :maxdepth: 2

  host-common
  host-misc
  host-filesystem
