# This doesn't work on sparc*-*-*.

set torture_eval_before_compile {
    global compiler_conditional_xfail_data
    set compiler_conditional_xfail_data {
      "PR target/12916" \
      { "sparc*-*-*" } \
      { "*" } \
      { "" }
    }
}

return 0
