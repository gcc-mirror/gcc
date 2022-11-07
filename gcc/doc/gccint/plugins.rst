..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Plugins

.. _plugins:

Plugins
-------

GCC plugins are loadable modules that provide extra features to the
compiler.  Like GCC itself they can be distributed in source and
binary forms.

GCC plugins provide developers with a rich subset of
the GCC API to allow them to extend GCC as they see fit.
Whether it is writing an additional optimization pass,
transforming code, or analyzing information, plugins
can be quite useful.

.. toctree::
  :maxdepth: 2

  plugins/loading-plugins
  plugins/plugin-api
  plugins/interacting-with-the-pass-manager
  plugins/interacting-with-the-gcc-garbage-collector
  plugins/giving-information-about-a-plugin
  plugins/registering-custom-attributes-or-pragmas
  plugins/recording-information-about-pass-execution
  plugins/controlling-which-passes-are-being-run
  plugins/keeping-track-of-available-passes
  plugins/building-gcc-plugins