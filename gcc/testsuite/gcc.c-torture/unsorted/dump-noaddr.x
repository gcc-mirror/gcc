# This checks if -fdump-noaddr dumps are done consistently.
proc dump_compare { src options } {
    global srcdir subdir
    global tmpdir
    
    exec echo $src
    
    global torture_with_loops
    set option_list $torture_with_loops
    set dumpbase dump-noaddr
    # ??? passing -dumpbase to the gcc driver doesn't work, since it will pass
    # another -dumpbase option to override it.
    # loop through all the options
    foreach option $option_list {
#	c-torture-compile ${dumpbase}_1 "$option $options -DMASK=1 -x c -da -fdump-tree-all"
#	c-torture-compile ${dumpbase}_2 "$option $options -DMASK=2 -x c -da -fdump-tree-all"
#	c-torture-compile ${dumpbase}_3 "$option $options -DMASK=3 -x c -da -fdump-tree-all"
	file delete -force dump1
	file delete -force dump2
	file mkdir dump1
	file mkdir dump2
	cd dump1
	c-torture-compile $src "$option $options -DMASK=1 -x c --param ggc-min-heapsize=1 -da -fdump-tree-all -fdump-noaddr"
	cd ../dump2
	c-torture-compile $src "$option $options -DMASK=2 -x c -da -fdump-tree-all -fdump-noaddr"
	cd ..
	foreach dump1 [lsort [glob -nocomplain dump1/*]] {
	    regsub dump1/ $dump1 dump2/ dump2
	    set dumptail "gcc.c-torture/unsorted/[file tail $dump1]"
	    #puts "$option $dump1"
	    set tmp [ diff "$dump1" "$dump2" ]
	    if { $tmp == 0 } {
		untested "$dumptail, $option comparison"
	    } elseif { $tmp == 1 } {
		pass "$dumptail, $option comparison"
	    } else {
		fail "$dumptail, $option comparison"
	    }
	    #exec diff $dump1 $dump2
	}
    }
    file delete -force dump1
    file delete -force dump2
}

catch {dump_compare $src $options} result
puts $result
verbose result
return 1
