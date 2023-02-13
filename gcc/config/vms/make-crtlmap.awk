# Generate the VMS crtl map
#	Copyright (C) 2011-2023 Free Software Foundation, Inc.

BEGIN {
    is_first = 1;
    maxlen=1;
    maxlen_name="??"
    prev=""
    ORS=""
}

# Remove comment and blank lines.
/^ *#/ || /^ *$/ {
  next;
}

{
    # Handle comma
    if (!is_first)
        print ",\n"
    else
        is_first = 0;

    # Check the map is sorted
    if ($0 <= prev)
    {
        print "Map not sorted!! (with name " $0 ")\n"
        exit 1
    }
    prev = $0

    # Compute the max of the identifier len.
    l=length($1)
    if (l > maxlen)
    {
        maxlen = l
        maxlen_name = $1
    }

    print "{ \"" $1 "\", "
    if (NF == 1)
        print "0 }"
    else
    {
	printf "VMS_CRTL_" $2
	for (i = 3; i <= NF; i++)
	    printf " | VMS_CRTL_" $i
	printf " }"
    }
}

END {
    print "\n\n"
    print "#define VMS_CRTL_MAXLEN " maxlen "  /*" maxlen_name " */\n"
}
