# sprintf() does not support %f on m6811/m6812 target.
if { [istarget "m6811-*-*"] || [istarget "m6812-*-*"]} {
	return 1
}
return 0
