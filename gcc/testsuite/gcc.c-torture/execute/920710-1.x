# h8300 does not have long long
if { [istarget "h8300-*-*"] } {
        return 1;
}

return 0
