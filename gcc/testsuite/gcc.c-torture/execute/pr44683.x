if { [istarget "sh*-*-*"] } {
	# SH require -mieee for this test.
	set additional_flags "-mieee"
}

return 0
