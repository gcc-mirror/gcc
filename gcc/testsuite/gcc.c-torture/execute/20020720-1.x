# The following targets fail 20020720-1.c for several reasons:
# (1) They implement floating point instructions via software, or
# (2) Their machine descriptions obfuscate the "abs(x) < 0.0" comparison.
#     For example, 1 insn performs abs, 1 insn loads 0.0, 1 insn sets
#     CCmode flags based upon the FP comparison, and a 4th insn makes
#     a conditional branch based upon the appropriate bits in the flags.
#     If the intermediate comparison can't be recognized, the combiner
#     is unable to optimize all four RTL instructions simultaneously.

if { [istarget "powerpc-*-*"] || [istarget "rs6000-*-*"] \
     || [istarget "mips*-*-*"] || [istarget "xtensa-*-*"] \
     || [istarget "sh-*-*"] } {
    return 1
}

return 0
