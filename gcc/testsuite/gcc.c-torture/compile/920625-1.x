# The problem on IA-64 is that if-conversion creates a sequence
#
#	(p17) cmp.geu p6, p7 = r48, r15
#	(p16) cmp.gtu p6, p7 = r48, r15
#
# where p16 and p17 are complemenary, but the assembler DV validation
# code doesn't recognize that p6 and p7 are complimentary, and so
# we end up warning for a later use
#
#	(p6) addl r14 = 1, r0
#	(p7) mov r14 = r0
#
# that appears to be a WAW violation.

set torture_eval_before_compile {

    set compiler_conditional_xfail_data {
        "missing .pred.rel.mutex directive" \
        "ia64-*-*" \
        { "-O2" "-O3" "-Os" } \
        { "" }
    }
}

return 0
