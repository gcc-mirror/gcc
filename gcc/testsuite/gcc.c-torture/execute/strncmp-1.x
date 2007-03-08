if {[istarget i?86-*-vxworks*]
    || [istarget mips*-*-vxworks*]
    || [istarget sh*-*-vxworks*]
    || [istarget sparc*-*-vxworks*]} {
    # The kernel strncmp doesn't perform unsigned comparisons.
    set torture_eval_before_execute {
	global compiler_conditional_xfail_data
	set compiler_conditional_xfail_data {
	    "The kernel strncmp doesn't perform unsigned comparisons."
	    { "*-*-*" }
	    {}
	    { "-mrtp" }
	}
    }
}
return 0
