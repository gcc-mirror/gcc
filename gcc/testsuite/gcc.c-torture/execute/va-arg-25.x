# With -Os we default to -mpreferred-stack-boundary=2, which is not
# enough for proper operation with V4SImode when the architecture
# default enables SSE.  Arguably setting -mpreferred-stack-boundary=2
# under this condition is incorrect.  Finding the correct set of 
# options such that we don't exchange a FAIL for an XPASS is hard;
# simply force the stack boundary we need and forget about it for now.

if { [istarget "i?86-*-*"] || [istarget "x86_64-*-*"] } {
	set additional_flags "-mpreferred-stack-boundary=4"
}

return 0
