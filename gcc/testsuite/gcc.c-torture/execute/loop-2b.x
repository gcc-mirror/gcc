# This test has failed off and on for ages, depending on the optimization
# level and the target.  A rewrite of the strength reduction code is really
# required.

set torture_eval_before_execute {

    set compiler_conditional_xfail_data {
        "strength reduction lossage" \
        "i?86-*-*" \
        { "-O2" } \
        { "" }
    }
}

return 0
