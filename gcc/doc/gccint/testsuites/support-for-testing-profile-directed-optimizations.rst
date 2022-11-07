..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _profopt-testing:

Support for testing profile-directed optimizations
**************************************************

The file :samp:`profopt.exp` provides language-independent support for
checking correct execution of a test built with profile-directed
optimization.  This testing requires that a test program be built and
executed twice.  The first time it is compiled to generate profile
data, and the second time it is compiled to use the data that was
generated during the first execution.  The second execution is to
verify that the test produces the expected results.

To check that the optimization actually generated better code, a
test can be built and run a third time with normal optimizations to
verify that the performance is better with the profile-directed
optimizations.  :samp:`profopt.exp` has the beginnings of this kind
of support.

:samp:`profopt.exp` provides generic support for profile-directed
optimizations.  Each set of tests that uses it provides information
about a specific optimization:

``tool``
  tool being tested, e.g., :command:`gcc`

``profile_option``
  options used to generate profile data

``feedback_option``
  options used to optimize using that profile data

``prof_ext``
  suffix of profile data files

``PROFOPT_OPTIONS``
  list of options with which to run each test, similar to the lists for
  torture tests

:samp:`{ dg-final-generate { {local-directive} } }`
  This directive is similar to ``dg-final``, but the
  :samp:`{local-directive}` is run after the generation of profile data.

:samp:`{ dg-final-use { {local-directive} } }`
  The :samp:`{local-directive}` is run after the profile data have been
  used.