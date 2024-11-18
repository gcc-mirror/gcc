.. Copyright (C) 2024 Free Software Foundation, Inc.
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

SARIF support
=============

`SARIF <https://www.sarif.info/>`_ is a machine-readable format, originally
designed for the output of static analysis tools, but which can be used
for diagnostics in general.

.. function:: void diagnostic_manager_add_sarif_sink (diagnostic_manager *diag_mgr, \
                                                      FILE *dst_stream, \
                                                      const diagnostic_file *main_input_file, \
                                                      enum diagnostic_sarif_version version)

   Add a new output sink to ``diag_mgr``, which writes SARIF of the given
   version to ``dst_stream``.

   The output is not written until ``diag_mgr`` is released.

   ``dst_stream`` is borrowed, and must outlive ``diag_mgr``.

   For the result to be a valid SARIF file according to the schema,
   ``diag_mgr`` must have had :func:`diagnostic_manager_set_tool_name`
   called on it.

   ``diag_mgr``, ``dst_stream``, and ``main_input_file`` must all be non-NULL.

  .. enum:: diagnostic_sarif_version

     An enum for choosing the SARIF version for a SARIF output sink.

     .. macro:: DIAGNOSTIC_SARIF_VERSION_2_1_0

     .. macro:: DIAGNOSTIC_SARIF_VERSION_2_2_PRERELEASE
