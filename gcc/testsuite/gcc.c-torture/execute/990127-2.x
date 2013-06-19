# On x86 targets, two floating-point double values can't be reliably compared
# for inequality due to excess precision of 80387 floating-point coprocessor.
# Use -mpc64 to force 80387 floating-point precision to 64 bits.  This option
# has no effect on SSE, but it is needed in case of -m32 on x86_64 targets.

if { [istarget i?86-*-darwin*]
     || [istarget i?86-*-linux*]
     || [istarget i?86-*-gnu*]
     || [istarget i?86-*-kfreebsd*-gnu]
     || [istarget i?86-*-knetbsd*-gnu]
     || [istarget i?86-*-solaris2*]
     || [istarget x86_64-*-darwin*]
     || [istarget x86_64-*-linux*]
     || [istarget x86_64-*-kfreebsd*-gnu]
     || [istarget x86_64-*-knetbsd*-gnu]
     || [istarget x86_64-*-solaris2*] } {
	set additional_flags "-mpc64"
}

return 0
