if [istarget "spu-*-*"] {
    # No Inf/NaN support on SPU.
    return 1
}
add-ieee-options
return 0
