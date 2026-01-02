.. Copyright (C) 2024-2026 Free Software Foundation, Inc.
   Originally contributed by David Malcolm <dmalcolm@redhat.com>

   This is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see
   <https://www.gnu.org/licenses/>.

.. default-domain:: c

Diagnostic Managers
===================

.. type:: diagnostic_manager;

A :type:`diagnostic_manager` is an opaque bundle of state for a client of
libgdiagnostics.

It has zero of more "output sinks" to which diagnostics are emitted.

Responsibilities include:

* location-management

* caching of source file content

* patch generation

.. function:: diagnostic_manager *diagnostic_manager_new (void)

   Create a new diagnostic_manager.
   The caller will need to call :func:`diagnostic_release_manager`
   on it at some point.

   .. note:: No output sinks are created by default; so you will want
      to create one with something like:

      .. code-block::

	diagnostic_manager_add_text_sink (diag_mgr, stderr,
                                          DIAGNOSTIC_COLORIZE_IF_TTY);

.. function::  void diagnostic_manager_release (diagnostic_manager *diag_mgr)

   Release a diagnostic_manager.

   This will flush output to all of the output sinks, and clean up.

   The parameter must be non-NULL.

.. function:: int diagnostic_manager_add_sink_from_spec (diagnostic_manager *affected_mgr, \
				       const char *option_name, \
				       const char *spec, \
				       diagnostic_manager *control_mgr)

   This function can be used to support option processing similar to GCC's
   `-fdiagnostics-add-output= <https://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Message-Formatting-Options.html#index-fdiagnostics-add-output>`_.
   This allows command-line tools to support the same domain-specific
   language for specifying output sinks as GCC does.

   The function will attempt to parse ``spec`` as if it were
   an argument to GCC's `-fdiagnostics-add-output= <https://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Message-Formatting-Options.html#index-fdiagnostics-add-output>`_.
   If successful, it will add an output sink to ``affected_mgr`` and return zero.
   Otherwise, it will emit an error diagnostic to ``control_mgr`` and
   return non-zero.

   ``affected_mgr`` and ``control_mgr`` can be the same manager,
   or be different managers.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_2`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_manager_add_sink_from_spec

.. function:: void diagnostic_manager_set_analysis_target (diagnostic_manager *mgr, \
	                                                   const diagnostic_file *file)

   This function sets the "main input file" of ``mgr`` to be
   ``file``.
   This affects the :code:`<title>` of generated HTML and
   the :code:`role` of the :code:`artifact` in SARIF output
   (`SARIF v2.1.0 section 3.24.6 <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790867>`_).

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_2`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_manager_set_analysis_target
