# This does not compile on HC11/HC12 due to the asm which requires
# two 32-bit registers.
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] } {
	return 1
}
return 0
