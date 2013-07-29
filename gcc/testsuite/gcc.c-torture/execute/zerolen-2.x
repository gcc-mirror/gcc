if [istarget "epiphany-*-*"] {
    # This test assumes the absence of larger-than-word padding.
    # to make this true for struct foo on epiphany would require
    # __attribute__((packed,aligned(__alignof__(word)))) .
    return 1
}
return 0
