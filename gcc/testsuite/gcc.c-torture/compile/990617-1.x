# 0x70000000 is too large a constant to become a pointer on stormy16.

if { [istarget "stormy16-*-*"] } {
        return 1;
}

return 0
