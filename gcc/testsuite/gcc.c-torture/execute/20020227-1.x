# This test reportedly fails on all 64-bit targets, see PR6221.  It's
# been observed to fail on at least mips-irix6, alpha, ia64, hppa64,
# sparc64/sparcv9 and mmix during April 2002.

if { [istarget "sparc64-*-*"] || [istarget "sparcv9-*-*"] } {
    # On sparc64/sparcv9 it doesn't fail at -O0/-O1, or at all with -m32.
    set torture_eval_before_execute {
        global compiler_conditional_xfail_data
        set compiler_conditional_xfail_data {
            "This test fails on 64-bit targets, see PR6221." \
            { "*-*-*" } \
            { "-O2" "-O3" "-Os" } \
            { "-m32" }
        }
    }
} elseif { [istarget "sparc-*-*"] } {
    # Regular sparc fails with -m64, but not with -O0/-O1.
    set torture_eval_before_execute {
        global compiler_conditional_xfail_data
        set compiler_conditional_xfail_data {
            "This test fails on 64-bit targets, see PR6221." \
            { "*-*-*" } \
            { "-m64" } \
            { "-O0" "-O1" }
        }
    }
} elseif { [istarget "powerpc64-*-*"] || [istarget "x86_64-*-*"] } {
    # PowerPC-64 and x86_64 do not fail at any optimization level.
} elseif { [istarget "*64*-*-*"] || [istarget "alpha*-*-*"]
        || [istarget "mmix-*-*"] || [istarget "mips*-*-irix6*"] } {
    # Other 64-bit targets fail at all optimization levels.
    set torture_execute_xfail [istarget]
}

return 0
