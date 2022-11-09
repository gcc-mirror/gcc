..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _internal-flags:

Internal flags controlling lto1
*******************************

The following flags are passed into :command:`lto1` and are not
meant to be used directly from the command line.

.. option:: -fwpa

  This option runs the serial part of the link-time optimizer
  performing the inter-procedural propagation (WPA mode).  The
  compiler reads in summary information from all inputs and
  performs an analysis based on summary information only.  It
  generates object files for subsequent runs of the link-time
  optimizer where individual object files are optimized using both
  summary information from the WPA mode and the actual function
  bodies.  It then drives the LTRANS phase.

.. option:: -fltrans

  This option runs the link-time optimizer in the
  local-transformation (LTRANS) mode, which reads in output from a
  previous run of the LTO in WPA mode.  In the LTRANS mode, LTO
  optimizes an object and produces the final assembly.

.. option:: -fltrans-output-list=file

  This option specifies a file to which the names of LTRANS output
  files are written.  This option is only meaningful in conjunction
  with :option:`-fwpa`.

.. option:: -fresolution=file

  This option specifies the linker resolution file.  This option is
  only meaningful in conjunction with :option:`-fwpa` and as option
  to pass through to the LTO linker plugin.
