# This test reportedly fails on all 64-bit targets, see PR6221.  It's
# been observed to fail on at least mips-irix6, alpha, ia64, hppa64,
# sparc64/sparcv9 and mmix during April 2002.

if { [istarget "*64*-*-*"] || [istarget "alpha*-*-*"] || [istarget "mmix-*-*"]
     || [istarget "sparcv9-*-*"] || [istarget "mips*-*-irix6*"] } {
    set torture_execute_xfail [istarget]
}

# Regular sparc- also fails with -m64.
set torture_eval_before_execute {
    global compiler_conditional_xfail_data
    set compiler_conditional_xfail_data {
        "This test fails on 64-bit targets, see PR6221." \
        { "sparc-*-*" } \
        { "-m64" } \
        { "" }
    }
}

return 0
