# This doesn't work on d10v if ints are not 32 bits

if { [istarget "d10v-*-*"] && ! [string-match "*-mint32*" $CFLAGS] } {
	set torture_execute_xfail "d10v-*-*"
}

return 0
