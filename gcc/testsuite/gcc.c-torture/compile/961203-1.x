# The structure is too large for the stormy16 - won't fit in 16 bits.

if { [istarget "stormy16-*-*"] } {
        return 1;
}

# Array 'a' in this test is too large to fit in 64K.

global target_triplet
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"]} {
	set torture_compile_xfail "$target_triplet"
}
return 0
