# The following targets fail 20020720-1.c for several reasons:
# (1) They implement floating point instructions via software, or
# (2) The "abs(x) < 0.0" comparison is split up in too many intermediate
#     insns for combine to handle -- it can handle max three insns
#     simultaneously.  For example, for RISCy machines it is common that 1
#     insn performs abs, 1 insn loads 0.0, 1 insn sets CCmode flags based
#     upon the FP comparison, and a 4th insn makes a conditional branch
#     based upon the appropriate bits in the flags.  On the other hand, if
#     for example the comparison insn takes 0.0 as an operand, the
#     combiner is able to see all intermediate instructions simultaneously
#     and can make the optimization.
if { [istarget "mips*-*-*"] || [istarget "xtensa-*-*"] \
     || [istarget "sh-*-*"] || [istarget "arm*-*-*"] \
     || [istarget "strongarm*-*-*"] || [istarget "xscale*-*-*"] \
     || [istarget "sparc64-*-*"] || [istarget "sparcv9-*-*"] } {
    set torture_execute_xfail [istarget]
}

# sparc-*-* also fails with -m64.
set torture_eval_before_execute {
    global compiler_conditional_xfail_data
    set compiler_conditional_xfail_data {
        "This test fails on sparc -m64, see PR8087." \
        { "sparc-*-*" } \
        { "-m64" } \
        { "" }
    }
}

return 0
