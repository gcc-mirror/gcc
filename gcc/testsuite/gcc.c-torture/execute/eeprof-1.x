if { [istarget "powerpc-ibm-aix*"] } {
        set torture_execute_xfail "powerpc-ibm-aix*"
}
set additional_flags "-finstrument-functions"
return 0
