if [istarget "spu-*-*"] {
    # This doesn't work on the SPU because single precision floats are
    # always rounded toward 0.
    return 1
}
return 0
