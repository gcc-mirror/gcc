.. Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

Message buffers
===============

.. type:: diagnostic_message_buffer

A :type:`diagnostic_message_buffer` is a buffer into which text can be
accumulated, before being used:

* as the message of a diagnostic, using :func:`diagnostic_finish_via_msg_buf`

* as the text of a label for a :type:`diagnostic_physical_location` using
  :func:`diagnostic_add_location_with_label_via_msg_buf`

* as the text of an event within a :type:`diagnostic_execution_path` using
  :func:`diagnostic_execution_path_add_event_via_msg_buf`

This is to allow more flexible creation of messages than a "format string
plus variadic arguments" API.

.. function:: diagnostic_message_buffer * diagnostic_message_buffer_new (void)

   This function creates a new :type:`diagnostic_message_buffer`.

   The caller is responsible for cleaning it up, either by handing it off
   to one of the API entrypoints that takes ownership of it (such as
   :func:`diagnostic_finish_via_msg_buf`), or by calling
   :func:`diagnostic_message_buffer_release` on it.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

.. function:: void diagnostic_message_buffer_release (diagnostic_message_buffer *msg_buf)

   This function releases ``msg_buf``.

   Typically you don't need to call this, but instead will pass the
   buffer to one of the API entrypoints that takes over ownership of
   it (such as :func:`diagnostic_finish_via_msg_buf`); calling it
   after this would lead to a double-free bug, as you no longer "own"
   the buffer.

   ``msg_buf`` must be non-NULL.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

.. function:: void diagnostic_message_buffer_append_str (diagnostic_message_buffer *msg_buf, \
				      const char *p)

   This function appends the null-terminated string ``p`` to the buffer.
   The string is assumed to be UTF-8 encoded.

   ``msg_buf`` and ``p`` must both be non-NULL.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

.. function:: void diagnostic_message_buffer_append_text (diagnostic_message_buffer *msg_buf, \
	       const char *p, \
	       size_t len)

   This function appends ``len`` bytes from ``p`` to the buffer.
   The bytes are assumed to be UTF-8 encoded.

   ``msg_buf`` and ``p`` must both be non-NULL.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

.. function:: void diagnostic_message_buffer_append_byte (diagnostic_message_buffer *msg_buf,\
				       char ch)

   This function appends ``ch`` to the buffer.  This should be either
   ASCII, or part of UTF-8 encoded text.

   ``msg_buf`` must be non-NULL.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

.. function:: void diagnostic_message_buffer_append_printf (diagnostic_message_buffer *msg_buf, \
					 const char *fmt, ...)

   This function appends a formatted string to the buffer, using the
   formatting rules for ``printf``.

   The string is assumed to be UTF-8 encoded.

   ``msg_buf`` and ``fmt`` must both be non-NULL.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

.. function:: void diagnostic_message_buffer_append_event_id (diagnostic_message_buffer *msg_buf, \
					   diagnostic_event_id event_id)

   This function appends a :type:`diagnostic_event_id` to the buffer.

   ``msg_buf`` must be non-NULL.

   For text output, the event will be printed in the form ``(1)``.

   This is analogous to the
   :doc:`%@ message formatting code <message-formatting>`.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

Hyperlink support
*****************

.. function:: void diagnostic_message_buffer_begin_url (diagnostic_message_buffer *msg_buf, \
				     const char *url)

   This function indicates the beginning of a run of text that should be
   associated with the given URL.  The run of text should be closed with
   a matching call to :func:`diagnostic_message_buffer_end_url`.

   ``msg_buf`` and ``url`` must both be non-NULL.

   For text output in a suitably modern terminal, the run of text will
   be emitted as a clickable hyperlink to the URL.

   For SARIF sinks, the run of text will be emitted using SARIF's
   embedded link syntax.

   This is analogous to the
   :doc:`%{ message formatting code <message-formatting>`.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

.. function:: void diagnostic_message_buffer_end_url (diagnostic_message_buffer *msg_buf)

   This function ends a run of text within the buffer started with
   :func:`diagnostic_message_buffer_begin_url`.

   ``msg_buf`` must be non-NULL.

   This is analogous to the
   :doc:`%} message formatting code <message-formatting>`.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

Quoted text
***********

.. function:: void diagnostic_message_buffer_begin_quote (diagnostic_message_buffer *msg_buf)

   This function indicates the beginning of a run of text that should be
   printed in quotes.  The run of text should be closed with
   a matching call to :func:`diagnostic_message_buffer_end_quote`.

   ``msg_buf`` must be non-NULL.

   For text output in a suitably modern terminal, the run of text will
   appear in bold.
   be emitted as a clickable hyperlink to the URL.

   For SARIF sinks, the run of text will be emitted using SARIF's
   embedded link syntax.

   This is analogous to the
   ``%<``:doc:`message formatting code <message-formatting>`.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

.. function:: void diagnostic_message_buffer_end_url (diagnostic_message_buffer *msg_buf)

   This function ends a run of text within the buffer started with
   :func:`diagnostic_message_buffer_begin_url`.

   ``msg_buf`` must be non-NULL.

   This is analogous to the
   :doc:`%> message formatting code <message-formatting>`.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

Color
*****

.. function:: void diagnostic_message_buffer_begin_color (diagnostic_message_buffer *msg_buf, \
				     const char *color)

   This function indicates the beginning of a run of text that should be
   colorized as the given color.  The run of text should be closed with
   a matching call to :func:`diagnostic_message_buffer_end_color`.

   The precise set of available color names is currently undocumented.

   ``msg_buf`` and ``color`` must both be non-NULL.

   For text output in a suitable terminal, the run of text will
   be colorized.

   For SARIF sinks, the run of text will be emitted using SARIF's
   embedded link syntax.

   This is analogous to the
   :doc:`%r message formatting code <message-formatting>`.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

.. function:: void diagnostic_message_buffer_end_color (diagnostic_message_buffer *msg_buf)

   This function ends a run of text within the buffer started with
   :func:`diagnostic_message_buffer_begin_color`.

   ``msg_buf`` must be non-NULL.

   This is analogous to the
   :doc:`%R message formatting code <message-formatting>`.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

Debugging a message buffer
**************************

.. function:: void diagnostic_message_buffer_dump (const diagnostic_message_buffer *msg_buf, \
				FILE *outf)

   This function writes a representation of the contents of ``msg_buf``
   to ``outf``, for debugging.

   ``msg_buf`` can be NULL or non-NULL.
   ``outf`` must be non-NULL.

   This function was added in :ref:`LIBGDIAGNOSTICS_ABI_4`; you can
   test for its presence using

   .. code-block:: c

      #ifdef LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer
