# This test fails at -O1 and higher.
set torture_eval_before_compile {
    global compiler_conditional_xfail_data
    set compiler_conditional_xfail_data {
        "Fails at all optimization levels but -O0, see PR10375." \
        { "*-*-*" } \
        { "-O*" } \
        { "-O0" }
    }
}

return 0
