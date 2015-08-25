load_lib target-supports.exp

if [istarget "nvptx-*-*"] {
    # This test uses memcpy for block move in the same file as it
    # defines it.  The two decls are not the same, by design, and we
    # end up emitting a definition of memcpy, along with a .extern
    # declaration. This confuses the ptx assembler.
    return 1
}
return 0
