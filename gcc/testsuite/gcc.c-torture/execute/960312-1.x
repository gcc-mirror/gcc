# This test fails on HC11/HC12 when it is compiled without -mshort because 
# is uses an asm that requires two 32-bit registers (int).  It passes
# when using -mshort because there are enough registers;  force -mshort.
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] } {
	set options "-mshort"
}
return 0
