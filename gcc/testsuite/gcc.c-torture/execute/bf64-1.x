# The MCore ABI specifies that bitfields may not exceed 32 bits.
# Hence this tes will fail.

if { [istarget "mcore-*-*"] } {
	set torture_execute_xfail "mcore-*-*"
}

return 0
