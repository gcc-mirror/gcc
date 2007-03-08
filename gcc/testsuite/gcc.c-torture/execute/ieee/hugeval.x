# This test fails under hpux 9.X and 10.X because HUGE_VAL is DBL_MAX
# instead of +Infinity.

global target_triplet
if { [istarget "hppa*-*-hpux9*"] || [istarget "hppa*-*-hpux10*"] } {
      set torture_execute_xfail "$target_triplet"
}

# VxWorks kernel mode has the same problem.
if {[istarget "*-*-vxworks*"]} {
    set torture_eval_before_execute {
	global compiler_conditional_xfail_data
	set compiler_conditional_xfail_data {
	    "The kernel HUGE_VAL is defined to DBL_MAX instead of +Inf."
	    { "*-*-*" }
	    {}
	    { "-mrtp" }
	}
    }
}

return 0

