# This does not work on m68hc11 or h8300 due to the use of an asm statement
# to force a 'long long' (64-bits) to go in a register.

if { [istarget "m6811-*-*"]
     || [istarget "m6812-*-*"]
     || [istarget "h8300-*-*"] } {
      return 1
}

return 0
