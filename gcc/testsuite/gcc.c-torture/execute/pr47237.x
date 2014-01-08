if { [istarget "nios2-*-*"] } {
    # This test can cause the stack to underflow on Nios II.
    set torture_execute_xfail [istarget]
}

return 0
