set options "-S"

# This does not work on m68hc11 due to the asm which forces a
# float or a double to go in a register.

global target_triplet
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] } {
      set torture_compile_xfail "$target_triplet"
      return 1
}
return 0
