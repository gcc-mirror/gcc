# This doesn't work on h8300s
# It also doesn't work on d10v if doubles are not 64 bits

if { [istarget "h8300*-*-*"] } {
	set torture_execute_xfail "h8300*-*-*"
}

if { [istarget "d10v-*-*"] && ! [string-match "*-mdouble64*" $CFLAGS] } {
	set torture_execute_xfail "d10v-*-*"
}

return 0
