# The memory checking code does not mark the stack as readable or writable
# so this test fails.  Ideally the memory checking library ought to 
# cooperate with the host OS to mark the stack as it is used or individual
# function prologues and epilogues ought to mark their pieces of stack as 
# writable and readable-after-written.

# Setting XFAIL here creates noise because the test passes at
# random optimization levels for quite a number of targets.
# Don't run the test at all.
return 1
