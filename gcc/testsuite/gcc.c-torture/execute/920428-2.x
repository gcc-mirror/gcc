# This doesn't work on sparc's with -mflat.

if { [istarget "sparc-*-*"] && [string match "*mflat*" $CFLAGS] } {
	set torture_execute_xfail "sparc-*-*"
}
return 0
