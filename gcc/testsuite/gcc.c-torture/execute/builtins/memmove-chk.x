load_lib target-supports.exp

if { ! [check_effective_target_nonlocal_goto] } {
    return 1
}

if [istarget "epiphany-*-*"] {
    # This test assumes the absence of struct padding.
    # to make this true for test5 struct A on epiphany would require
    # __attribute__((packed)) .
    return 1
}
return 0
