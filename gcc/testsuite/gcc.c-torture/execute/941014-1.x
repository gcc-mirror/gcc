# This doesn't always work for Thumb.
 
if { [istarget arm*-*-*] || [istarget xscale*-*-*] \
     || [istarget strongarm*-*-*] } {
    set torture_eval_before_execute {
	global compiler_conditional_xfail_data
	set compiler_conditional_xfail_data {
	    "Thumb sets the last bit of function relocations" \
		    { "arm*-*-*" "xscale*-*-*" "strongarm*-*-*" } \
		    { { "-mthumb" "-O0" } } \
		    { "" }
	}
    }
}

return 0
