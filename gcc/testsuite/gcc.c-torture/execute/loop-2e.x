# This doesn't work on m68k-motorola-sysv
# It also doesn't work on m88k-motorola-sysv3

global target_triplet
if { [istarget "m68k-motorola-sysv"] || [istarget "m88k-motorola-sysv3"] } {
      set torture_compile_xfail "$target_triplet"
}

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
