# This does not work on m68hc11 due to the use of an asm statement
# to force a 'long long' (64-bits) to go in a register.

if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] } {
      return 1
}

return 0
