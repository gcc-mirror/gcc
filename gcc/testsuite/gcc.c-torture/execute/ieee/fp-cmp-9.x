# The ARM VxWorks kernel uses an external floating-point library in
# which routines like __ledf2 are just aliases for __cmpdf2.  These
# routines therefore don't handle NaNs correctly.
if [istarget "arm*-*-vxworks*"] {
    set torture_eval_before_execute {
	global compiler_conditional_xfail_data
	set compiler_conditional_xfail_data {
	    "The ARM kernel uses a flawed floating-point library."
	    { "*-*-*" }
	    { "-O0" }
	    { "-mrtp" }
	}
    }
}

return 0
