# On IA-64 the assembler may emit
#
# Warning: Additional NOP may be necessary to workaround Itanium
# processor A/B step errata
#
# This can be fixed by adding "-mb-step" to the command line, which
# does in fact add the extra nop.

if [istarget "ia64-*-*"] {
    set torture_eval_before_compile {
        set option "$option -mb-step"
    }
}
return 0
