# This does not work on m68hc11 due to the asm statement which
# forces two 'long' (32-bits) variables to go in registers.

if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] } {
      return 1
}
return 0
