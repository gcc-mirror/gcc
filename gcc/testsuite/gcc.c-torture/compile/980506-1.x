# The arrays are too large for the xstormy16 - won't fit in 16 bits.
if { [istarget "xstormy16-*-*"] } {
        return 1;
}

return 0
