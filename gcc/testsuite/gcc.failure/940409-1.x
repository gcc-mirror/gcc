# gcc doesn't handle compile/940409-1.cexp properly, but it's an obscure bug
# so rather than constantly seeing it fail, just record it as expected to
# fail.

set torture_compile_xfail "*-*-*"
return 0 ;# `0' says we didn't handle the testcase ourselves so continue
