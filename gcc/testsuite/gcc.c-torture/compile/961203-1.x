# This doesn't work on any host with 32 bit int or smaller.

# Because this test tends to consume lots of system resources and doesn't
# currently work, don't actually run it.  Just report a failure.
setup_xfail "*-*-*"
fail "gcc.c-torture/compile/961203-1.c"
return 1 ;# `1' says we handled the testcase ourselves
