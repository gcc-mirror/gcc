# This does not work on m68hc11 due to the asm statement which
# forces two 'long' (32-bits) variables to go in registers.

global target_triplet
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] } {
      set torture_compile_xfail "$target_triplet"
}
return 0
