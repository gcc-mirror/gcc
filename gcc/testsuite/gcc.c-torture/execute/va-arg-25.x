# This doesn't work on SPARC 64-bit.

if { [istarget "sparc64-*-*"] || [istarget "sparcv9-*-*"] } {
    set torture_eval_before_compile {
        global compiler_conditional_xfail_data
        set compiler_conditional_xfail_data {
            "PR target/12916" \
            { "*-*-*" } \
            { "*" } \
            { "-m32" }
        }
    }
} elseif { [istarget "sparc-*-*"] } {
    set torture_eval_before_compile {
        global compiler_conditional_xfail_data
        set compiler_conditional_xfail_data {
             "PR target/12916" \
            { "*-*-*" } \
            { "-m64" } \
            { "" }
        }
    }
}

return 0
