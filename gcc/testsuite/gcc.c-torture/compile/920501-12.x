# This test fails on HC11/HC12 when it is compiled without -mshort because 
# the stack arrays are too large.  Force to use 16-bit ints for it.
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] } {
	set options "-mshort"
}
return 0
