# This does not assemble on m68hc11 because the function is larger
# than 64K.

global target_triplet
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] } {
      set torture_compile_xfail "$target_triplet"
      return 1
}
return 0
