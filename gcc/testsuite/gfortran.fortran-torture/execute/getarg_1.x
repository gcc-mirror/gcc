if [istarget "spu-*-*"] {
    # We need -mstdmain to enable argument processing on SPU.
    lappend additional_flags "-mstdmain"
}
return 0
