load_lib target-supports.exp

set additional_flags "-fno-strict-overflow"

if { [check_effective_target_int16] } {
	return 1
}

return 0
