set torture_eval_before_compile {
    set compiler_conditional_xfail_data {
        "bug with -funroll-loops" \
        "i?86-*-*" \
        { "-funroll-loops" "-funroll-all-loops" } \
        { "" }
    }
}

return 0
