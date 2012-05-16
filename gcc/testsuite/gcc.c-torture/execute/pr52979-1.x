load_lib target-supports.exp

if { [check_effective_target_int32plus] } {
	return 0
}

return 1;
