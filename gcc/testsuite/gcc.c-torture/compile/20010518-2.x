# This test fails on HC11/HC12 when it is compiled without -mshort because 
# the array is too large (INT_MAX/2 > 64K).  Force to use 16-bit ints for it.
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] } {
	set options "-S -mshort"
} else {
	set options "-S"
}
return 0
