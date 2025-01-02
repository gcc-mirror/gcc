.. Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

Adding metadata
===============

Tool metadata
*************

It's possible to set up various metadata on the :type:`diagnostic_manager`
as a whole, describing the program creating the diagnostics.

.. note::

   It's not required to set up any of this up on a
   :type:`diagnostic_manager`.  However, if you are doing
   :doc:`SARIF output <sarif>`, then you need to at least call
   :func:`diagnostic_manager_set_tool_name` or the generated ``.sarif``
   file will not validate against the schema.

.. function:: void diagnostic_manager_set_tool_name (diagnostic_manager *diag_mgr, \
                                                     const char *value)

   Set a string for the name of the tool emitting the diagnostics.

   Both parameters must be non-NULL.

   If set, this string will be used

   * by :doc:`text output sinks <text-output>` as a prefix for output
     when no physical location is available, replacing ``progname``
     in the following:

     .. code-block:: console

	$ ./tut01-hello-world
	progname: error: I'm sorry Dave, I'm afraid I can't do that

   * by :doc:`SARIF output sinks <sarif>` as the value for the
     ``name`` property of the ``driver``
     (`SARIF v2.1.0 §3.19.8 <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790791>`_).

.. function:: void diagnostic_manager_set_full_name (diagnostic_manager *diag_mgr, \
                                                      const char *value)

   Set a string giving the name of the tool along with the its version and
   other useful information::

     diagnostic_manager_set_full_name (diag_mgr, "FooChecker 0.1 (en_US)");

   If set, this string will be used by :doc:`SARIF output sinks <sarif>` as
   the value for the ``fullName`` property of the ``driver``
   (`SARIF v2.1.0 §3.19.9 <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790792>`_).

   Both parameters must be non-NULL.

.. function:: void diagnostic_manager_set_version_string (diagnostic_manager *diag_mgr, \
                                                          const char *value)

   Set a string suitable for use as the value of the SARIF ``version`` property
   of the ``driver``.
   (`SARIF v2.1.0 §3.19.13 <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790796>`_)::

     diagnostic_manager_set_version_string (diag_mgr, "0.1");

   Both parameters must be non-NULL.

.. function:: void diagnostic_manager_set_version_url (diagnostic_manager *diag_mgr, \
                                                       const char *value)

   Set a string suitable for use as the value of the SARIF ``informationUri``
   property of the ``driver``.
   (`SARIF v2.1.0 §3.19.17 <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790800>`_)::

     diagnostic_manager_set_version_url (diag_mgr,
                                         "https://www.example.com/foo-checker/releases/0.1/");

  Both parameters must be non-NULL.

Adding metadata to a diagnostic
*******************************

.. function:: void diagnostic_set_cwe (diagnostic *diag, \
                                       unsigned cwe_id)

   Associate ``diag`` with the given ID within
   the `Common Weakness Enumeration <https://cwe.mitre.org/>`_::

     /* CWE-242: Use of Inherently Dangerous Function.  */
     diagnostic_set_cwe (d, 242);

   ``diag`` must be non-NULL.

   The CWE value will be printed by text sinks after the message::

     test-metadata.c:21:3: warning: never use 'gets' [CWE-242]

   and in a sufficiently-capable terminal will be a link to
   documentation about the CWE.

.. function:: void diagnostic_add_rule (diagnostic *diag, \
                                        const char *title, \
                                        const char *url)

   Associate this :type:`diagnostic` with a particular rule that has been
   violated (such as in a coding standard, or within a specification).

   A diagnostic can be associated with zero or more rules.

   ``diag`` must be non-NULL.  The rule must have at least one of a
   title and a URL, but these can be NULL.

   For example, given::

     diagnostic_add_rule (d,
                          "MSC24-C",
                          "https://wiki.sei.cmu.edu/confluence/display/c/MSC24-C.+Do+not+use+deprecated+or+obsolescent+functions");

   the rule name will be printed by text sinks after the message::

     test-metadata.c:21:3: warning: never use 'gets' [MSC24-C]
      21 |   gets (buf);
         |   ^~~~~~~~~~

   and if so, the URL will be available in a sufficiently capable
   terminal.

   This can be used in conjunction with :func:`diagnostic_set_cwe`,
   giving output like this::

     test-metadata.c:21:3: warning: never use 'gets' [CWE-242] [MSC24-C]
      21 |   gets (buf);
         |   ^~~~~~~~~~
