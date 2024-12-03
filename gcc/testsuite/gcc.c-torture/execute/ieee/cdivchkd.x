if [istarget "avr-*-*"] {
    # Floating-point support is incomplete.
    return 1
}
return 0
