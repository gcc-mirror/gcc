if {[istarget "csky-*-*"] && [check_effective_target_hard_float]} {
    # The C-SKY hardware FPU only supports flush-to-zero mode.
    set torture_execute_xfail "csky-*-*"
    return 1
}
if [istarget "epiphany-*-*"] {
    # The Epiphany single-precision floating point format does not
    # support subnormals.
    return 1
}
if {[istarget "m68k-*-*"] && [check_effective_target_coldfire_fpu]} {
    # ColdFire FPUs require software handling of subnormals.  We are
    # not aware of any system that has this.
    set torture_execute_xfail "m68k-*-*"
}
if { [istarget "tic6x-*-*"] && [check_effective_target_ti_c67x] } {
    # C6X floating point hardware turns denormals to zero in multiplications.
    set torture_execute_xfail "tic6x-*-*"
    return 1
}
return 0
