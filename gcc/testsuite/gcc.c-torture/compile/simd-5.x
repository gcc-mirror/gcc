# h8300 does not have long long
if { [istarget "h8300-*-*"] } {
    return 1;
}

if { [istarget "sparc64-*-*"] || [istarget "sparcv9-*-*"] } {
    # On SPARC64/SPARC-V9 it fails, except with -m32.
    set torture_eval_before_compile {
        global compiler_conditional_xfail_data
        set compiler_conditional_xfail_data {
            "PR target/9200" \
            { "*-*-*" } \
            { "*" } \
            { "-m32" }
        }
    }
} elseif { [istarget "sparc-*-*"] } {
    # On regular SPARC it doesn't fail, except with -m64.
    set torture_eval_before_compile {
        global compiler_conditional_xfail_data
        set compiler_conditional_xfail_data {
            "PR target/9200" \
            { "*-*-*" } \
            { "-m64" } \
            { "" }
        }
    }
} elseif { [istarget "powerpc64-*-*"] } {
    # On PowerPC-64 it fails unconditionally.
    set torture_eval_before_compile {
        global compiler_conditional_xfail_data
        set compiler_conditional_xfail_data {
            "PR target/9680" \
            "*-*-*" \
            { "*" } \
            { "" }
        }
    }
}

return 0
