..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: makefile fragment

.. _fragments:

Makefile Fragments
------------------

When you configure GCC using the :samp:`configure` script, it will
construct the file :samp:`Makefile` from the template file
:samp:`Makefile.in`.  When it does this, it can incorporate makefile
fragments from the :samp:`config` directory.  These are used to set
Makefile parameters that are not amenable to being calculated by
autoconf.  The list of fragments to incorporate is set by
:samp:`config.gcc` (and occasionally :samp:`config.build`
and :samp:`config.host`); See :ref:`system-config`.

Fragments are named either :samp:`t-{target}` or :samp:`x-{host}`,
depending on whether they are relevant to configuring GCC to produce
code for a particular target, or to configuring GCC to run on a
particular host.  Here :samp:`{target}` and :samp:`{host}` are mnemonics
which usually have some relationship to the canonical system name, but
no formal connection.

If these files do not exist, it means nothing needs to be added for a
given target or host.  Most targets need a few :samp:`t-{target}`
fragments, but needing :samp:`x-{host}` fragments is rare.

.. toctree::
  :maxdepth: 2

  target-makefile-fragments
  host-makefile-fragments
