# This doesn't work on PAs

if { [istarget "hppa*-*-*"] } {
	set torture_execute_xfail "hppa*-*-*"
}

return 0
