# This doesn't work on d10v if doubles are not 64 bits

if { [istarget "d10v-*-*"] && ! [string-match "*-mdouble64*" $CFLAGS] } {
	set torture_execute_xfail "d10v-*-*"
}

# freebsd sets up the fpu with a different precision control which causes
# this test to "fail".
if { [istarget "i?86-*-freebsd*\[12345\]\.*"] } {
	set torture_execute_xfail "i?86-*-freebsd*"
}
return 0
