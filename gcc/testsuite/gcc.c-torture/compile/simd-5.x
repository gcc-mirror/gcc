# h8300 does not have long long
if { [istarget "h8300-*-*"] } {
    return 1;
}

# PowerPC-64 doesn't handle this; see PR target/9680
set torture_eval_before_compile {
    set compiler_conditional_xfail_data {
        "PR target/9680" \
        "powerpc64-*-*" \
        { "*" } \
        { "" }
    }
}

return 0
