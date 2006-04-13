load_lib target-supports.exp

if { [check_effective_target_int16] } {
	return 1
}

# m32c pointers can be 24 bits in a 32 bit variable, so the test
# patterns may get truncated.
if { [istarget "m32c-*-*"] } {
	return 1
}


return 0

