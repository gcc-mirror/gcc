# This does not assemble on m68hc11 because the function is larger
# than 64K.

global target_triplet
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] } {
      return 1
}

# It doesn't work on Xtensa with -O0 because the function is larger
# than the range of a jump instruction (+- 128K) and the assembler
# does not yet relax jumps to indirect jumps.

set torture_eval_before_compile {
    set compiler_conditional_xfail_data {
        "jump beyond 128K not supported" \
        "xtensa-*-*" \
        { "-O0" } \
        { "" }
    }
}

return 0
