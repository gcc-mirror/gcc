# 0x70000000 is too large a constant to become a pointer on xstormy16.

if { [istarget "xstormy16-*-*"] } {
        return 1;
}

return 0
