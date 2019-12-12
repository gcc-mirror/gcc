if { [check_effective_target_newlib_nano_io] } {
    lappend additional_flags "-Wl,-u,_printf_float"
}
return 0
