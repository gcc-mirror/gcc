# This test fails under hpux 9.X and 10.X because HUGE_VAL is DBL_MAX
# instead of +Infinity.

global target_triplet
if { [istarget "hppa*-*-hpux9*"] || [istarget "hppa*-*-hpux10*"] } {
      set torture_execute_xfail "$target_triplet"
}

return 0

