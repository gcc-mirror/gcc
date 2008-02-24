# This test only works if PREFERRED_STACK_BOUNDARY >= 128.
if { [istarget mips*-*-*] } {
   return 1
}
return 0
