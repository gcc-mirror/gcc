# Scratch files aren't implemented for mmixware
# (_stat is a stub and files can't be deleted).
# Similar restrictions exist for most simulators.

if { [istarget "mmix-knuth-mmixware"]
     || [istarget "cris-*-elf"] } {
	set torture_execute_xfail [istarget]
}

return 0
