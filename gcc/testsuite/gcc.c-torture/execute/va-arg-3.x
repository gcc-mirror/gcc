# This doesn't work on mn10200

if { [istarget "mn10200*-*-*"] } {
	set torture_execute_xfail "mn10200*-*-*"
}

if { [istarget "h8300*-*-*"] } {
	set torture_execute_xfail "h8300*-*-*"
}


return 0
