if [istarget "avr-*-*"] {
    # AVR doubles are floats
    return 1
}
return 0
