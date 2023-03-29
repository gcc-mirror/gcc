#!/bin/bash
# Copyright (C) 2006-2023 Free Software Foundation, Inc.
#
# Analyze changes in GCC DejaGNU test logs for binutils, gcc, gdb, etc.
# Original version written in 2005 by James Lemke <jwlemke@wasabisystems.com>.
#
# See usage() below.

usage () {
    cat <<EOF >&2
Usage:
    dg-cmp-results.sh [-v] [-v] [-v] <variant-name> <old-file> <new-file>
    <variant-name> names the desired variant, "/" must be written as "\/".
    Use the empty string ("") for the first variant in each file.
    Output is to stdout.
    Non-verbose output is degradation info like PASS->FAIL.
    -v adds improvement info like FAIL->PASS.
    -v -v adds info like tests that are no longer run.
    -v -v -v adds info for tests that have not changed status.
    -v -v -v -v is used for debugging.
EOF
}

verbose=0
while test "$1" = "-v"; do
    verbose=`expr $verbose + 1`
    shift
done

if test $# -ne 3 ; then
    usage
    exit 1
fi

if test ! -f "$2"; then
    echo "unable to open $2" >&2
    exit 1
fi

if test ! -f "$3"; then
    echo "unable to open $3" >&2
    exit 1
fi

# Command differences for various platforms.
case `uname -s` in
Darwin|NetBSD)
    E=-E	# sed
    ;;
*)
    E=-r	# sed
    ;;
esac

# sections are identified by separator lines beginning with '\t\t==='.
# section 0 identifies run date, target, and host.
# section 1 and subsequent contain test data for a target variant.
# -skip to /^Running target/ and use that line to identify the variant.
# -subsequent lines contain the result data.  They begin with:
# '(PASS|FAIL|XFAIL|XPASS|UNTESTED|UNSUPPORTED|UNRESOLVED):'
VARIANT="$1"
OFILE="$2"
OBASE=`basename "$2"`
NFILE="$3"
NBASE=`basename "$3"`
TMPDIR=${TMPDIR:-/tmp}

echo "dg-cmp-results.sh: Verbosity is ${verbose}, Variant is \"${VARIANT}\""
echo

header="^Running target $VARIANT"

temp=`grep "$header" $OFILE`
if test -z "$temp"; then
    echo "Error: variant \"$VARIANT\" not found in $OFILE."
    exit 1
fi
temp=`grep "$header" $NFILE`
if test -z "$temp"; then
    echo "Error: variant \"$VARIANT\" not found in $NFILE."
    exit 1
fi
unset temp

# Copy out the old file's section 0.
echo "Older log file: $OFILE"
sed $E -e '/^[[:space:]]+===/,$d' $OFILE

# Copy out the new file's section 0.
echo "Newer log file: $NFILE"
sed $E -e '/^[[:space:]]+===/,$d' $NFILE

# Create a temporary file from the old file's interesting section.
sed $E -e "/$header/,/^[[:space:]]+===.*Summary ===/!d" \
  -e '/^[A-Z]+:/!d' \
  -e '/^(WARNING|ERROR):/d' \
  -e 's/\r$//' \
  -e 's/^/O:/' \
  $OFILE |
  sort -s -t : -k 3b - \
  >$TMPDIR/o$$-$OBASE

# Create a temporary file from the new file's interesting section.
sed $E -e "/$header/,/^[[:space:]]+===.*Summary ===/!d" \
  -e '/^[A-Z]+:/!d' \
  -e '/^(WARNING|ERROR):/d' \
  -e 's/\r$//' \
  -e 's/^/N:/' \
  $NFILE |
  sort -s -t : -k 3b - \
  >$TMPDIR/n$$-$NBASE

# Merge the two files, then compare adjacent lines.
# Comparison is complicated by tests that may be run multiple times.
# If that case, we assume that the order is the same in both files.
cat <<EOF >compare-$$.awk
BEGIN {
    FS = ":"
    queue1 = 1; queueN = 0; status[queue1] = ""; name[queue1] = ""
    verbose = verbose + 0	# Make sure it's defined.
}

# FIFO circular queue
function push(st, nm) {
    queueN += 1; status[queueN] = st; name[queueN] = nm
}
function peek() {
    result = 0
    if (queueN >= queue1) result = queue1
    return result
}
function drop() {
    queue1 += 1
    if (queue1 > queueN) { queue1 = 1; queueN = 0; }
}

function compare(st, nm) {
    old = peek()
    if (old == 0) {
	# This new test wasn't run last time.
	if(st == "FAIL" || st == "UNRESOLVED" || verbose >= 2) {
	    # New test fails or we want all changes
	    printf("NA->%s:%s\n", st, nm)
	}
    }
    else {
	# Compare this new test to the first queued old one.
	if (verbose >= 4) {
	    printf("Comparing two lines:\n O:%s:%s\n N:%s:%s\n",
	     status[old], name[old], st, nm)
	}
        if (name[old] != nm) {
	    # The old test wasn't run this time and
	    # the new test wasn't run last time.
	    if (verbose >= 2) {
		printf("%s->NA:%s\n", status[old], name[old])
		if (nm != "") printf("NA->%s:%s\n", st, nm)
	    }
	    drop()
        }
	else {
	    notable = 0
	    if (status[old] == st) {
	        # Status of this test has not changed.
		if (verbose >= 3) printf("%s:%s\n", st, nm)
	    }
	    else if(status[old] == "PASS" && st == "XFAIL") {
	        if (verbose >= 1) notable = 1
	    }
	    else if(status[old] == "PASS" || st == "FAIL") {
	        # Test did pass but doesn't now
		# or didn't fail but does now.
		notable = 1
	    }
	    else if(st == "PASS") {
	        # Test didn't pass but does now.
		if (verbose >= 1) notable = 1
	    }
	    else if(verbose >= 2) {
	        # Miscellaneous status change.
		notable = 1
	    }
	    if (notable > 0) printf("%s->%s:%s\n", status[old], st, nm)
	    drop()
	}
    }
}

/^O:/ {
    while (old = peek()) {
	if (name[old] == \$3) break;
	# The queued test is no longer run.
	compare("", "");
    }
    # Save this test for later comparison.
    push(\$2, \$3)
}

/^N:/ {
    compare(\$2, \$3)
}

END {
    while (old = peek()) compare("", "")
}
EOF
sort -m -s -t : -k 3b $TMPDIR/o$$-$OBASE $TMPDIR/n$$-$NBASE |
 awk -v verbose=$verbose -f compare-$$.awk /dev/stdin

# Delete the temporary files.
rm -f compare-$$.awk $TMPDIR/o$$-$OBASE $TMPDIR/n$$-$NBASE

exit 0
