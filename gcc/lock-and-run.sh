#! /bin/sh
# Shell-based mutex using mkdir.  This script is used in make to prefer
# serialized execution to avoid consuming too much RAM.  If reusing it,
# bear in mind that the lock-breaking logic is not race-free, so disable
# it in err() if concurrent execution could cause more serious problems.

self=`basename $0`
lockdir="$1" prog="$2"; shift 2 || exit 1

# Remember when we started trying to acquire the lock.
count=0

err () {
    if test -f $lockdir/lock-$1.$$; then
	rm -rf $lockdir
	echo "$self: *** (PID $$) removed stale $lockdir" >&2

	# Possible variant for uses where races are more problematic:
	#echo "$self: *** (PID $$) giving up, maybe rm -r $lockdir" >&2
	#exit 42
    else
	touch $lockdir/lock-$1.$$
    fi
}

until mkdir "$lockdir" 2>/dev/null; do
    # Say something periodically so the user knows what's up.
    if [ `expr $count % 30` = 0 ]; then
	# Check for valid lock.
	if pid=`cat $lockdir/pid 2>/dev/null` && kill -0 $pid 2>/dev/null; then
	    echo "$self: (PID $$) waiting $count sec to acquire $lockdir from PID $pid" >&2
	elif test -z "$pid"; then
	    echo "$self: (PID $$) cannot read $lockdir/pid" >&2
	    err nopid
	else
	    echo "$self: (PID $$) cannot signal $lockdir owner PID $pid" >&2
	    err dead
	fi
    fi
    sleep 1
    count=`expr $count + 1`
done

trap 'rm -rf "$lockdir"' 0
echo $$ > $lockdir/pidT && mv $lockdir/pidT $lockdir/pid
echo "$self: (PID $$) acquired $lockdir after $count seconds" >&2

echo $prog "$@"
$prog "$@"

# The trap runs on exit.
