# This does not work on m68hc11 due to the use of an asm statement
# to force a 'long long' (64-bits) to go in a register.

global target_triplet
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] } {
      set torture_compile_xfail "$target_triplet"
      return 1
}

return 0
