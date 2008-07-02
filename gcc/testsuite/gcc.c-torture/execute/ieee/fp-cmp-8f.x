if [istarget "spu-*-*"] {
    # The SPU single-precision floating point format does not
    # support Nan & Inf. 
    return 1
}
return 0
