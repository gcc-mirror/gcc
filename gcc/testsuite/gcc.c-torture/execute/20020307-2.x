# This doesn't work on MIPS Irix, see PR6222

if { [istarget "mips*-sgi-irix6*"] } {
	set torture_execute_xfail [istarget]
}

return 0
