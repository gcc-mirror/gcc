# This only fails for `-O2', but not `-O3' or `-Os'
# as I had expected.   See clues in testcase source file.

set torture_eval_before_execute {

    set compiler_conditional_xfail_data {
        "loop optimizer leaves dangling pseudo" \
	"i?86-*-*" \
	{ "-O2" } \
	{ "" }
	}
}

return 0
