# The array is too big.
if { [istarget "h8300-*-*"] } {
        return 1;
}

if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] } {
        return 1;
}

return 0
