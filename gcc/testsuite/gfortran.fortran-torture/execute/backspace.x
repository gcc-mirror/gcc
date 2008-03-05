load_lib target-supports.exp

if { ! [check_effective_target_fd_truncate] } {
	return 1
}

return 0
