..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: environment variables

.. _environment-variables:

Environment Variables
---------------------

This section describes the environment variables that affect how CPP
operates.  You can use them to specify directories or prefixes to use
when searching for include files, or to control dependency output.

Note that you can also specify places to search using options such as
:option:`-I`, and control dependency output with options like
:option:`-M` (see :ref:`invocation`).  These take precedence over
environment variables, which in turn take precedence over the
configuration of GCC.

.. include:: ../../../doc/cppenv.rst