# This doesn't work on MIPS Irix.
# See http://gcc.gnu.org/ml/gcc-patches/2002-04/msg00473.html

if { [istarget "mips*-sgi-irix6*"] } {
	set torture_execute_xfail "mips*-sgi-irix6*"
}

return 0
