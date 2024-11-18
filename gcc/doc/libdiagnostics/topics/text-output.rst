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

Text output
===========

.. type:: diagnostic_text_sink

.. function:: diagnostic_text_sink * diagnostic_manager_add_text_sink (diagnostic_manager *diag_mgr,\
                                                                       FILE *dst_stream, \
                                                                       enum diagnostic_colorize colorize)

   Add a new output sink to ``diag_mgr``, which writes GCC-style diagnostics
   to ``dst_stream``.
   Return a borrowed pointer to the sink, which is cleaned up when ``diag_mgr``
   is released.

   ``diag_mgr`` must be non-NULL.

   ``dst_stream`` must be non-NULL.  It is borrowed and must outlive ``DIAG_MGR``.

   The output for each diagnostic is written and flushed as each
   :type:`diagnostic` is finished.

   .. enum:: diagnostic_colorize

      An enum for determining if we should colorize a text output sink.

      .. macro:: DIAGNOSTIC_COLORIZE_IF_TTY

	 Diagnostics should be colorized if the destination stream is
	 directly connected to a tty.

      .. macro:: DIAGNOSTIC_COLORIZE_NO

	 Diagnostics should not be colorized.

      .. macro:: DIAGNOSTIC_COLORIZE_YES

	 Diagnostics should be colorized.

.. function:: void diagnostic_text_sink_set_source_printing_enabled (diagnostic_text_sink *text_sink, \
                                                                     int value)

   Enable or disable printing of source text in the text sink.

   ``text_sink`` must be non-NULL.

   Default: enabled.

.. function:: void diagnostic_text_sink_set_colorize (diagnostic_text_sink *text_sink, \
                                                      enum diagnostic_colorize colorize)

   Update colorization of text sink.

   ``text_sink`` must be non-NULL.

.. function:: void diagnostic_text_sink_set_labelled_source_colorization_enabled (diagnostic_text_sink *text_sink, \
                                                                                  int value)

   ``text_sink`` must be non-NULL.

   Enable or disable colorization of the characters of source text
   that are underlined.

   This should be true for clients that generate range information
   (so that the ranges of code are colorized), and false for clients that
   merely specify points within the source code (to avoid e.g. colorizing
   just the first character in a token, which would look strange).

   Default: enabled.
