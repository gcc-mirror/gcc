if [istarget "spu-*-*"] {
    # No denormal support on SPU.
    return 1
}
add-ieee-options
return 0
