# Various intrinsics not implemented and not implementable; will fail at
# link time.

if { [istarget "mmix-knuth-mmixware"] } {
	set torture_compile_xfail "mmix-knuth-mmixware"
}

return 0
