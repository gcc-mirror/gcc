# The arrays are too large for the stormy16 - won't fit in 16 bits.
if { [istarget "stormy16-*-*"] } {
        return 1;
}

return 0
