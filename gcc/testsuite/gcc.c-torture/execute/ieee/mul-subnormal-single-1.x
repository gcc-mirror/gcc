if [istarget "mips-sgi-irix6*"] {
    # IRIX 6 sets the MIPS IV flush to zero bit by default, so this test
    # isn't expected to work for n32 and n64 on MIPS IV targets.
    return 1
}
if {[istarget "m68k-*-linux-gnu*"] && [check_effective_target_coldfire_fpu]} {
    # ColdFire FPUs require software handling of subnormals.  Linux 2.6.10
    # does not have this.
    set torture_execute_xfail "m68k-*-linux-gnu*"
}
return 0
