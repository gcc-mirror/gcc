global target_triplet

if { ![istarget "*mips*"] } {
	return 1
} else {
	set torture_compile_xfail "$target_triplet"
}

return 0

