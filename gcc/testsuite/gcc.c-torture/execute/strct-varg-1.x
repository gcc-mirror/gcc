# This doesn't work on mn10200

if { [istarget "mn10200*-*-*"] } {
	set torture_execute_xfail "mn10200*-*-*"
}

return 0
