# This doesn't work on sparc*-*-* at -O0.

set torture_eval_before_compile {
    global compiler_conditional_xfail_data
    set compiler_conditional_xfail_data {
      "PR target/12916" \
      { "sparc*-*-*" } \
      { "-O0" } \
      { "" }
    }
}

return 0
