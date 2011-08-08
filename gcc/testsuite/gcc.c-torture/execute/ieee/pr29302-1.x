if { [istarget "tic6x-*-*"] && [check_effective_target_ti_c67x] } {
    # C6X uses -freciprocal-math by default.
    set torture_execute_xfail "tic6x-*-*"
    return 1
}
return 0
if { [istarget "tic6x-*-*"] && [check_effective_target_ti_c67x] } {
    # C6X uses -freciprocal-math by default.
    set torture_execute_xfail "tic6x-*-*"
    return 1
}
return 0
