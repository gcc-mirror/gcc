# Various intrinsics not implemented and not implementable; will fail at
# link time.

if { [istarget "mmix-knuth-mmixware"]
     || [istarget "arm*-*-elf"]
     || [istarget "strongarm*-*-elf"]
     || [istarget "xscale*-*-elf"]
     || [istarget "cris-*-elf"] } {
	set torture_compile_xfail [istarget]
}

return 0
