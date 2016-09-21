# This checks if -fdump-noaddr dumps are done consistently.
proc dump_compare { src options } {
    global srcdir subdir
    global tmpdir

    global torture_with_loops
    set option_list $torture_with_loops
    set dumpbase [file tail $src]

    # loop through all the options
    foreach option $option_list {
	file delete -force dump1
	file mkdir dump1
	c-torture-compile $src "$option $options -dumpbase dump1/$dumpbase -DMASK=1 -x c --param ggc-min-heapsize=1 -fdump-ipa-all -fdump-rtl-all -fdump-tree-all -fdump-noaddr"
	file delete -force dump2
	file mkdir dump2
	c-torture-compile $src "$option $options -dumpbase dump2/$dumpbase -DMASK=2 -x c -fdump-ipa-all -fdump-rtl-all -fdump-tree-all -fdump-noaddr"
	foreach dump1 [lsort [glob -nocomplain dump1/*]] {
	    regsub dump1/ $dump1 dump2/ dump2
	    set dumptail "gcc.c-torture/unsorted/[file tail $dump1]"
	    regsub {\.\d+((t|r|i)\.[^.]+)$} $dumptail {.*\1} dumptail
	    set tmp [ diff "$dump1" "$dump2" ]
	    if { $tmp == 0 } {
		untested "$dumptail, $option comparison"
	    } elseif { $tmp == 1 } {
		pass "$dumptail, $option comparison"
	    } else {
		fail "$dumptail, $option comparison"
	    }
	}
    }
    file delete -force dump1
    file delete -force dump2
}

dump_compare $src $options
return 1
