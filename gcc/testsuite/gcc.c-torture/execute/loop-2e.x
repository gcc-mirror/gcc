global target_triplet
if { [istarget "i?86-*"] } {
    set torture_eval_before_execute {
	global compiler_conditional_xfail_data
	set compiler_conditional_xfail_data {
	    "Loop optimiser bug" \
		    "i?86-*" \
		    { "-Os" } \
		    { "" }
	}
    }
}

return 0
