if [istarget "epiphany-*-*"] {
    # The Epiphany single-precision floating point format does not
    # support subnormals.
    return 1
}
if [istarget "mips-sgi-irix6*"] {
    # IRIX 6 sets the MIPS IV flush to zero bit by default, so this test
    # isn't expected to work for n32 and n64 on MIPS IV targets.
    return 1
}
if {[istarget "m68k-*-*"] && [check_effective_target_coldfire_fpu]} {
    # ColdFire FPUs require software handling of subnormals.  We are
    # not aware of any system that has this.
    set torture_execute_xfail "m68k-*-*"
}
if [istarget "spu-*-*"] {
    # The SPU single-precision floating point format does not
    # support subnormals.
    return 1
}
if { [istarget "tic6x-*-*"] && [check_effective_target_ti_c67x] } {
    # C6X floating point hardware turns denormals to zero in multiplications.
    set torture_execute_xfail "tic6x-*-*"
    return 1
}
return 0
