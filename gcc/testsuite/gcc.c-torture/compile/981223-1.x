# The problem on IA-64 is that the assembler emits
#
# Warning: Additional NOP may be necessary to workaround Itanium
# processor A/B step errata
#
# This can be fixed by adding "-mb-step" to the command line, which
# does in fact add the extra nop, if someone can tell me how to do
# that for a c-torture compile test.

set torture_eval_before_compile {

    set compiler_conditional_xfail_data {
        "need -mb-step" \
        "ia64-*-*" \
        { "-O2" "-O3" "-Os" } \
        { "" }
    }
}

return 0
