load_lib target-supports.exp

# XFAIL: PR libobjc/36610, for targets which pass arguments via registers
# For powerpc-darwin it fails with -fgnu-runtime, passes with -fnext-runtime,
# but that would be too ugly to handle; let it fail there.

if { ([istarget x86_64-*-linux*] && [check_effective_target_lp64] )
     || [istarget powerpc*-*-linux*]
     || [istarget powerpc*-*-aix*]
     || [istarget s390*-*-*-linux*]
     || [istarget sh4-*-linux*]
     || [istarget hppa*-*-linux*]
     || [istarget hppa*-*-hpux*]
     || [istarget ia64-*-linux*] } {
    set torture_execute_xfail "*-*-*"
}

return 0
