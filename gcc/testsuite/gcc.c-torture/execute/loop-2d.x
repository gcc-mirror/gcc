if { [istarget "i686-*"] } {
    set torture_eval_before_execute {
	global compiler_conditional_xfail_data
	set compiler_conditional_xfail_data {
	    "Loop optimiser bug" \
		    "i686-*" \
		    { "-Os" } \
		    { "" }
	}
    }
}

return 0
