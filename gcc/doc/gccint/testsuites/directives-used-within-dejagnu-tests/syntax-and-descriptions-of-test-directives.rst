..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _directives:

Syntax and Descriptions of test directives
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Test directives appear within comments in a test source file and begin
with ``dg-``.  Some of these are defined within DejaGnu and others
are local to the GCC testsuite.

The order in which test directives appear in a test can be important:
directives local to GCC sometimes override information used by the
DejaGnu directives, which know nothing about the GCC directives, so the
DejaGnu directives must precede GCC directives.

Several test directives include selectors (see :ref:`selectors`)
which are usually preceded by the keyword ``target`` or ``xfail``.

Specify how to build the test
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-do {do-what-keyword} [{ target/xfail {selector} }] }`
  :samp:`{do-what-keyword}` specifies how the test is compiled and whether
  it is executed.  It is one of:

  ``preprocess``
    Compile with :option:`-E` to run only the preprocessor.

  ``compile``
    Compile with :option:`-S` to produce an assembly code file.

  ``assemble``
    Compile with :option:`-c` to produce a relocatable object file.

  ``link``
    Compile, assemble, and link to produce an executable file.

  ``run``
    Produce and run an executable file, which is expected to return
    an exit code of 0.

  The default is ``compile``.  That can be overridden for a set of
  tests by redefining ``dg-do-what-default`` within the ``.exp``
  file for those tests.

  If the directive includes the optional :samp:`{ target {selector} }`
  then the test is skipped unless the target system matches the
  :samp:`{selector}`.

  If :samp:`{do-what-keyword}` is ``run`` and the directive includes
  the optional :samp:`{ xfail {selector} }` and the selector is met
  then the test is expected to fail.  The ``xfail`` clause is ignored
  for other values of :samp:`{do-what-keyword}` ; those tests can use
  directive ``dg-xfail-if``.

Specify additional compiler options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-options {options} [{ target {selector} }] }`
  This DejaGnu directive provides a list of compiler options, to be used
  if the target system matches :samp:`{selector}`, that replace the default
  options used for this set of tests.

:samp:`{ dg-add-options {feature} ... }`
  Add any compiler options that are needed to access certain features.
  This directive does nothing on targets that enable the features by
  default, or that don't provide them at all.  It must come after
  all ``dg-options`` directives.
  For supported values of :samp:`{feature}` see :ref:`add-options`.

:samp:`{ dg-additional-options {options} [{ target {selector} }] }`
  This directive provides a list of compiler options, to be used
  if the target system matches :samp:`{selector}`, that are added to the default
  options used for this set of tests.

Modify the test timeout value
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The normal timeout limit, in seconds, is found by searching the
following in order:

* the value defined by an earlier ``dg-timeout`` directive in
  the test

* variable :samp:`{tool_timeout}` defined by the set of tests

* :samp:`{gcc}`, :samp:`{timeout}` set in the target board

* 300

:samp:`{ dg-timeout {n} [{target {selector} }] }`
  Set the time limit for the compilation and for the execution of the test
  to the specified number of seconds.

:samp:`{ dg-timeout-factor {x} [{ target {selector} }] }`
  Multiply the normal time limit for compilation and execution of the test
  by the specified floating-point factor.

Skip a test for some targets
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-skip-if {comment} { {selector} } [{ {include-opts} } [{ {exclude-opts} }]] }`
  Arguments :samp:`{include-opts}` and :samp:`{exclude-opts}` are lists in which
  each element is a string of zero or more GCC options.
  Skip the test if all of the following conditions are met:

  * the test system is included in :samp:`{selector}`

  * for at least one of the option strings in :samp:`{include-opts}`,
    every option from that string is in the set of options with which
    the test would be compiled; use :samp:`"*"` for an :samp:`{include-opts}` list
    that matches any options; that is the default if :samp:`{include-opts}` is
    not specified

  * for each of the option strings in :samp:`{exclude-opts}`, at least one
    option from that string is not in the set of options with which the test
    would be compiled; use :samp:`""` for an empty :samp:`{exclude-opts}` list;
    that is the default if :samp:`{exclude-opts}` is not specified

  For example, to skip a test if option ``-Os`` is present:

  .. code-block:: c++

    /* { dg-skip-if "" { *-*-* }  { "-Os" } { "" } } */

  To skip a test if both options ``-O2`` and ``-g`` are present:

  .. code-block:: c++

    /* { dg-skip-if "" { *-*-* }  { "-O2 -g" } { "" } } */

  To skip a test if either ``-O2`` or ``-O3`` is present:

  .. code-block:: c++

    /* { dg-skip-if "" { *-*-* }  { "-O2" "-O3" } { "" } } */

  To skip a test unless option ``-Os`` is present:

  .. code-block:: c++

    /* { dg-skip-if "" { *-*-* }  { "*" } { "-Os" } } */

  To skip a test if either ``-O2`` or ``-O3`` is used with ``-g``
  but not if ``-fpic`` is also present:

  .. code-block:: c++

    /* { dg-skip-if "" { *-*-* }  { "-O2 -g" "-O3 -g" } { "-fpic" } } */

:samp:`{ dg-require-effective-target {keyword} [{ target {selector} }] }`
  Skip the test if the test target, including current multilib flags,
  is not covered by the effective-target keyword.
  If the directive includes the optional :samp:`{ {selector} }`
  then the effective-target test is only performed if the target system
  matches the :samp:`{selector}`.
  This directive must appear after any ``dg-do`` directive in the test
  and before any ``dg-additional-sources`` directive.
  See :ref:`effective-target-keywords`.

:samp:`{ dg-require-{support} args }`
  Skip the test if the target does not provide the required support.
  These directives must appear after any ``dg-do`` directive in the test
  and before any ``dg-additional-sources`` directive.
  They require at least one argument, which can be an empty string if the
  specific procedure does not examine the argument.
  See :ref:`require-support`, for a complete list of these directives.

Expect a test to fail for some targets
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-xfail-if {comment} { {selector} } [{ {include-opts} } [{ {exclude-opts} }]] }`
  Expect the test to fail if the conditions (which are the same as for
  ``dg-skip-if``) are met.  This does not affect the execute step.

:samp:`{ dg-xfail-run-if {comment} { {selector} } [{ {include-opts} } [{ {exclude-opts} }]] }`
  Expect the execute step of a test to fail if the conditions (which are
  the same as for ``dg-skip-if``) are met.

Expect the compiler to crash
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-ice {comment} [{ {selector} } [{ {include-opts} } [{ {exclude-opts} }]]] }`
  Expect the compiler to crash with an internal compiler error and return
  a nonzero exit status if the conditions (which are the same as for
  ``dg-skip-if``) are met.  Used for tests that test bugs that have not been
  fixed yet.

Expect the test executable to fail
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-shouldfail {comment} [{ {selector} } [{ {include-opts} } [{ {exclude-opts} }]]] }`
  Expect the test executable to return a nonzero exit status if the
  conditions (which are the same as for ``dg-skip-if``) are met.

Verify compiler messages
~~~~~~~~~~~~~~~~~~~~~~~~

Where :samp:`{line}` is an accepted argument for these commands, a value of :samp:`0`
can be used if there is no line associated with the message.

:samp:`{ dg-error {regexp} [{comment} [{ target/xfail {selector} } [{line}] ]] }`
  This DejaGnu directive appears on a source line that is expected to get
  an error message, or else specifies the source line associated with the
  message.  If there is no message for that line or if the text of that
  message is not matched by :samp:`{regexp}` then the check fails and
  :samp:`{comment}` is included in the ``FAIL`` message.  The check does
  not look for the string :samp:`error` unless it is part of :samp:`{regexp}`.

:samp:`{ dg-warning {regexp} [{comment} [{ target/xfail {selector} } [{line}] ]] }`
  This DejaGnu directive appears on a source line that is expected to get
  a warning message, or else specifies the source line associated with the
  message.  If there is no message for that line or if the text of that
  message is not matched by :samp:`{regexp}` then the check fails and
  :samp:`{comment}` is included in the ``FAIL`` message.  The check does
  not look for the string :samp:`warning` unless it is part of :samp:`{regexp}`.

:samp:`{ dg-message {regexp} [{comment} [{ target/xfail {selector} } [{line}] ]] }`
  The line is expected to get a message other than an error or warning.
  If there is no message for that line or if the text of that message is
  not matched by :samp:`{regexp}` then the check fails and :samp:`{comment}` is
  included in the ``FAIL`` message.

:samp:`{ dg-note {regexp} [{comment} [{ target/xfail {selector} } [{line}] ]] }`
  The line is expected to get a :samp:`note` message.
  If there is no message for that line or if the text of that message is
  not matched by :samp:`{regexp}` then the check fails and :samp:`{comment}` is
  included in the ``FAIL`` message.

  By default, any *excess* :samp:`note` messages are pruned, meaning
  their appearance doesn't trigger *excess errors*.
  However, if :samp:`dg-note` is used at least once in a testcase,
  they're not pruned and instead must *all* be handled explicitly.
  Thus, if looking for just single instances of messages with
  :samp:`note:` prefixes without caring for all of them, use
  :samp:`dg-message "note: [...]"` instead of :samp:`dg-note`, or use
  :samp:`dg-note` together with :samp:`dg-prune-output "note: "`.

:samp:`{ dg-bogus {regexp} [{comment} [{ target/xfail {selector} } [{line}] ]] }`
  This DejaGnu directive appears on a source line that should not get a
  message matching :samp:`{regexp}`, or else specifies the source line
  associated with the bogus message.  It is usually used with :samp:`xfail`
  to indicate that the message is a known problem for a particular set of
  targets.

:samp:`{ dg-line {linenumvar} }`
  This DejaGnu directive sets the variable :samp:`{linenumvar}` to the line number of
  the source line.  The variable :samp:`{linenumvar}` can then be used in subsequent
  ``dg-error``, ``dg-warning``, ``dg-message``, ``dg-note``
  and ``dg-bogus``
  directives.  For example:

  .. code-block:: c++

    int a;   /* { dg-line first_def_a } */
    float a; /* { dg-error "conflicting types of" } */
    /* { dg-message "previous declaration of" "" { target *-*-* } first_def_a } */

:samp:`{ dg-excess-errors {comment} [{ target/xfail {selector} }] }`
  This DejaGnu directive indicates that the test is expected to fail due
  to compiler messages that are not handled by :samp:`dg-error`,
  :samp:`dg-warning`, ``dg-message``, :samp:`dg-note` or
  :samp:`dg-bogus`.
  For this directive :samp:`xfail`
  has the same effect as :samp:`target`.

:samp:`{ dg-prune-output {regexp} }`
  Prune messages matching :samp:`{regexp}` from the test output.

Verify output of the test executable
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-output {regexp} [{ target/xfail {selector} }] }`
  This DejaGnu directive compares :samp:`{regexp}` to the combined output
  that the test executable writes to :samp:`stdout` and :samp:`stderr`.

Specify environment variables for a test
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-set-compiler-env-var {var_name} "{var_value}" }`
  Specify that the environment variable :samp:`{var_name}` needs to be set
  to :samp:`{var_value}` before invoking the compiler on the test file.

:samp:`{ dg-set-target-env-var {var_name} "{var_value}" }`
  Specify that the environment variable :samp:`{var_name}` needs to be set
  to :samp:`{var_value}` before execution of the program created by the test.

Specify additional files for a test
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-additional-files "{filelist}" }`
  Specify additional files, other than source files, that must be copied
  to the system where the compiler runs.

:samp:`{ dg-additional-sources "{filelist}" }`
  Specify additional source files to appear in the compile line
  following the main test file.

Add checks at the end of a test
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-final { {local-directive} } }`
  This DejaGnu directive is placed within a comment anywhere in the
  source file and is processed after the test has been compiled and run.
  Multiple :samp:`dg-final` commands are processed in the order in which
  they appear in the source file.  See :ref:`final-actions`, for a list
  of directives that can be used within ``dg-final``.
