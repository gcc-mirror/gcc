# This doesn't work for thumb-elf
 
if { [istarget "thumb-*-elf"] } {
        set torture_execute_xfail "thumb-*-elf"
}
return 0
