load_lib target-supports.exp

# XFAIL: PR libobjc/36610, for targets which pass arguments via registers

if { ([istarget x86_64-*-linux*] && [check_effective_target_lp64] )
     || [istarget powerpc*-*-linux*]
     || [istarget powerpc*-*-aix*]
     || [istarget s390*-*-*-linux*]
     || [istarget sh4-*-linux*]
     || [istarget hppa*-*-linux*]
     || [istarget hppa*-*-hpux*]
     || [istarget ia64-*-linux*] } {
    set torture_execute_xfail "*-*-*"
}

# For darwin and alpha-linux it fails with -fgnu-runtime,
# passes with -fnext-runtime.

if { ([istarget x86_64-*-darwin*] && [check_effective_target_lp64] )
     || [istarget powerpc*-*-darwin*]
     || [istarget alpha*-*-linux*] } {
    set torture_eval_before_execute {
	global compiler_conditional_xfail_data
	set compiler_conditional_xfail_data {
	    "Target fails with -fgnu-runtime" \
		"*-*-*" \
		{ "-fgnu-runtime" } \
		{ "" }
	}
    }
}

return 0
