set options "-S"

# This does not work on m68hc11 due to the asm which forces a
# float or a double to go in a register.

if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"] } {
      return 1
}
return 0
