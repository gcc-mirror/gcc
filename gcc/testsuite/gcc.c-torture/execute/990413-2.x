# This test is x86 specific.
if { [istarget "i?86-*-*"] || [istarget "x86_64-*-*"] } { return 0 }
return 1
