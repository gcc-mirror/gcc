#! /bin/sh
# Shell-based mutex using mkdir.

lockdir="$1" prog="$2"; shift 2 || exit 1

# Remember when we started trying to acquire the lock.
count=0
touch lock-stamp.$$

trap 'rm -r "$lockdir" lock-stamp.$$' 0

until mkdir "$lockdir" 2>/dev/null; do
    # Say something periodically so the user knows what's up.
    if [ `expr $count % 30` = 0 ]; then
	# Reset if the lock has been renewed.
	if [ -n "`find \"$lockdir\" -newer lock-stamp.$$`" ]; then
	    touch lock-stamp.$$
	    count=1
	# Steal the lock after 5 minutes.
	elif [ $count = 300 ]; then
	    echo removing stale $lockdir >&2
	    rm -r "$lockdir"
	else
	    echo waiting to acquire $lockdir >&2
	fi
    fi
    sleep 1
    count=`expr $count + 1`
done

echo $prog "$@"
$prog "$@"

# The trap runs on exit.
