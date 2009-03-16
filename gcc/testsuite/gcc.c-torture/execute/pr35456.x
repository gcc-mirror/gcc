# VAX does not support signed zero.
if [istarget "vax-*-*"] { return 1 }
return 0
