# This used to fail on ia32, with or without -ffloat-store.
# It works now, but some people think that's a fluke, so I'm
# keeping this around just in case.

#set torture_eval_before_execute {
#
#    set compiler_conditional_xfail_data {
#        "ia32 fp rounding isn't pedantic" \
#        "i?86-*-*" \
#        { "-O3" "-O2" "-O1" "-Os"} \
#        { "" }
#        }    
#}

return 0
