set torture_eval_before_execute {

    set compiler_conditional_xfail_data {
        "ia32 fp rounding isn't pedantic" \
        "i?86-*-*" \
        { "-O3" "-O2" "-O1" "-Os"} \
        { "" }
        }    
}

return 0
