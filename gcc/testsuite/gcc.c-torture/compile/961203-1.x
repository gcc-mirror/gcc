# Array 'a' in this test is too large to fit in 64K.

global target_triplet
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"]} {
	set torture_compile_xfail "$target_triplet"
}
return 0
