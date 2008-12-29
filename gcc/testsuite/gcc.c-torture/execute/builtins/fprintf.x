load_lib target-supports.exp

if { [check_effective_target_freestanding] } {
        return 1; 
}

return 0;
