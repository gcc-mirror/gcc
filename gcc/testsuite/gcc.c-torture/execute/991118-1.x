# PR 16437.  This fails on x86_64 now.
if { [istarget "x86_64-*-*"] } {
    set torture_eval_before_execute {
	global compiler_conditional_xfail_data
	set compiler_conditional_xfail_data {
	    { "PR 16437" } \
	    { "x86_64-*-*" } \
	    { "*-O[123s]*" } \
	    { "-O0" }
	}
    }
}

return 0
