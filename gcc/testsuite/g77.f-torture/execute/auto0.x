# This test fails compilation in cross-endian environments, for example as
# below, with a "sorry" message.

if { [ishost "i\[34567\]86-*-*"] } {
    if { [istarget "mmix-knuth-mmixware"] } {
	set torture_compile_xfail [istarget]
    }
}

return 0
