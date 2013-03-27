load_lib target-supports.exp

if { [ check_effective_target_nonpic ] } {
        return 0
}

return 1
