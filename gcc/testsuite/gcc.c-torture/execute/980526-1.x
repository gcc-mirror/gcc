set torture_eval_before_execute {

    set compiler_conditional_xfail_data {
        "I sure wish I knew why this was hosed (arm-elf is OK now)" \
	"*arm-*-coff *arm-*-pe thumb-*-coff thumb-*-pe fr30-*-elf" \
	{"-O3"} \
	{"" }
	}    
}

return 0
