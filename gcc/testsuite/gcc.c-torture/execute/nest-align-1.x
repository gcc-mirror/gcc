# Force bigger stack alignment for PowerPC EABI targets.
if { [istarget "powerpc-*-eabi*"] } {
    set additional_flags "-mno-eabi"
}
return 0
