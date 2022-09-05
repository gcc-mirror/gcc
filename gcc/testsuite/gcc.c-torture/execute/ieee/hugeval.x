# This test fails under hpux 9.X and 10.X because HUGE_VAL is DBL_MAX
# instead of +Infinity.

global target_triplet

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

if { [istarget "tic6x-*-*"] && [check_effective_target_ti_c67x] } {
    # C6X uses -freciprocal-math by default.
    set torture_execute_xfail "$target_triplet"
    return 1
}

return 0

