# This doesn't work on irix6.x or solaris2.[78] 
# GNATS PR 3743

global target_triplet
if { [istarget "mips*-sgi-irix6*"] || [istarget "sparc-sun-solaris2.8"] } {
	set torture_compile_xfail "$target_triplet"
	return 1
}

return 0
