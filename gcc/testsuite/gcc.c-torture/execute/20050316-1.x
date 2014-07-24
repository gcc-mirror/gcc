load_lib target-supports.exp

if { [check_effective_target_int16] } {
	return 1
}

set additional_flags "-Wno-psabi"
return 0;
