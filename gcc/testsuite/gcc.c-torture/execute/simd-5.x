if { [istarget "sparc64-*-*"] || [istarget "sparcv9-*-*"] } {
    set torture_eval_before_compile {
        global compiler_conditional_xfail_data
        set compiler_conditional_xfail_data {
            "PR middle-end/9200" \
            { "*-*-*" } \
            { "-O0" } \
            { "-m32" }
        }
    }
} elseif { [istarget "sparc-*-*"] } {
    set torture_eval_before_compile {
        global compiler_conditional_xfail_data
        set compiler_conditional_xfail_data {
            "PR middle-end/9200" \
            { "*-*-*" } \
            { "-m64" } \
            { "-O1" "-O2" "-O3" "-Os" }
        }
    }
}

return 0
