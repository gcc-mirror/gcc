if [istarget "epiphany-*-*"] {
    # This test assumes the absence of struct padding.
    # to make this true for test5 struct A on epiphany would require
    # __attribute__((packed)) .
    return 1
}
return 0
