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
if { [istarget "powerpc-*-*"] || [istarget "rs6000-*-*"] \
     || [istarget "mips*-*-*"] || [istarget "xtensa-*-*"] \
     || [istarget "sh-*-*"] || [istarget "mmix-*-*"] } {
    return 1
}

return 0
