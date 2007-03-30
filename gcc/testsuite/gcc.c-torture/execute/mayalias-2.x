set torture_eval_before_compile {
    set compiler_conditional_xfail_data {
        "PR 28834" \
        { "*-*-*" } \
        { "-O3 -g" } \
        { "" }
    }
}

return 0
