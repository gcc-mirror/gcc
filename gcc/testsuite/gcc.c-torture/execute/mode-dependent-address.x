load_lib target-supports.exp

if { ! [check_effective_target_stdint_types] } {
	return 1
}

return 0
