set torture_eval_before_compile {

    set compiler_conditional_xfail_data {
	"PR fortran/9972" \
	{ "*-*-*" } \
	{ "-O0" } \
	{ "" }
	}
}

return 0
