# freebsd sets up the fpu with a different precision control which causes
# this test to "fail".
if { [istarget "i?86-*-freebsd*\[123\]\.*"] } {
	set torture_execute_xfail "i?86-*-freebsd*"
}
return 0
