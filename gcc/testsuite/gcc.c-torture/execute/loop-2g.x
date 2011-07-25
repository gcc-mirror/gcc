load_lib target-supports.exp

if { ! [check_effective_target_mmap] } {
        return 1
}

if [istarget "m68k-*-linux*"] {
    # the executable is at the same position the test tries to remap
    return 1
}
return 0
