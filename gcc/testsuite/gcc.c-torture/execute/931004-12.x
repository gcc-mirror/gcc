if { [istarget "powerpc-*-darwin*] } {
    # xfail this on powerpc-*-darwin, see PR 15923
    set torture_execute_xfail [istarget]
}

return 0
