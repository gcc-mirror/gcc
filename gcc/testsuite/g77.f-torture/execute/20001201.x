# Scratch files aren't implemented for mmixware
# (_stat is a stub and files can't be deleted).

if { [istarget "mmix-knuth-mmixware"] } {
	set torture_execute_xfail "mmix-knuth-mmixware"
}

return 0
