# This does not assemble on m68hc11 because the function is larger
# than 64K.

# It doesn't work on Xtensa with -O0 because the function is larger
# than the range of a jump instruction (+- 128K) and the assembler
# does not yet relax jumps to indirect jumps.

global target_triplet
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] || [istarget "xtensa-*-*"]} {
      set torture_compile_xfail "$target_triplet"
      return 1
}
return 0
