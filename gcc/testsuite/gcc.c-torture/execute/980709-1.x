# pow() is not available on m6811/m6812 target, this test will not link.
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"]} {
	return 1
}
return 0
