load_lib target-supports.exp

if { [check_effective_target_int16] } {
	return 1
}

return 0
