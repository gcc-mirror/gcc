# This test does not compile on mips-irix6 using the native assembler,
# though it does work with gas.  See PR6200.  Since we cannot (???)
# distinguish which assembler is being used, always pass -S for irix.

if { [istarget "mips*-*-irix*"] } { set options "-S" }

return 0
