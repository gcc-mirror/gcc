# This test examines the attribute support for DLLs.
# Only COFF/PE formats support DLLs, (plus, as a special case
# the mcore-elf toolchain), so the code here tries to determine
# the file format and decide whether the test should be marked
# as unsupported.

set torture_eval_before_compile {

    if ![istarget "mcore-*-elf"] {
       
        set objformat [gcc_target_object_format]
    
        if { $objformat != "pe" } {
            unsupported "dll.c"
            return 1
        }
    }
}

return 0
