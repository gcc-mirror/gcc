# This doesn't work on m68k-motorola-sysv
# It also doesn't work on m88k-motorola-sysv3

global target_triplet
if { [istarget "m68k-motorola-sysv"] || [istarget "m88k-motorola-sysv3"] } {
      set torture_compile_xfail "$target_triplet"
}

return 0
