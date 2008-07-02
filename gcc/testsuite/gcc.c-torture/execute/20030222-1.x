if [istarget "spu-*-*"] {
    # Using inline assembly to convert long long to int is not working quite
    # right # on the SPU.  An extra shift-left-4-byte is needed.
    return 1
}
return 0
