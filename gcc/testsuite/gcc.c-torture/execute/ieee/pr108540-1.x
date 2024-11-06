load_lib target-supports.exp

if { ! [check_effective_target_double64] } {
    return 1
}

return 0
