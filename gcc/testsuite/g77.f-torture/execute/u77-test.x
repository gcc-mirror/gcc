# Various intrinsics not implemented and not implementable; will fail at
# link time.

if { [istarget "mmix-knuth-mmixware"]
     || [istarget "cris-*-elf"] } {
	set torture_compile_xfail [istarget]
}

return 0
