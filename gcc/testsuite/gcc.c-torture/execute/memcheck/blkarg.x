# The memeory checking code does not mark the stack as readable or writable
# so this test fails.  Ideally the memory checking library ought to 
# cooperate with the host OS to mark the stack as it is used or individual
# function prologues and epilogues ought to mark their pieces of stack as 
# writable and readable-after-written.

set torture_execute_xfail "*-*-*"

return 0
