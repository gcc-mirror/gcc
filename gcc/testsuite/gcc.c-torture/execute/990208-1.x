# Doesn't work at -O3 because of ifcvt.c optimizations which
# cause the 2 inlined labels to be at the same location.

set torture_eval_before_execute {

    set compiler_conditional_xfail_data {
	"ifcvt transforms 2 inlined labels to the same address" \
	{ "ia64-*-*" "arm*-*-*" "strongarm*-*-*" "xscale*-*-*" } \
	{ "-O3" } \
	{ "" }
	}
}

return 0
