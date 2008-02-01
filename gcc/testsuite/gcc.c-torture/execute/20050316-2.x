# This testcase generates MMX instructions together with x87 instructions.
# Currently, there is no "emms" generated to switch between register sets,
# so the testcase fails for targets where MMX insns are enabled.

if { [istarget "i?86-*-*"] || [istarget "x86_64-*-*"] } {
	set additional_flags "-mno-mmx"
}

return 0
