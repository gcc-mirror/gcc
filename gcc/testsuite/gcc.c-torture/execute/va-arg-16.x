# This doesn't work on MIPS Irix.

if { [istarget "mips-sgi-irix6.5"] } {
	set torture_execute_xfail "mips-sgi-irix6.5"
}

return 0
