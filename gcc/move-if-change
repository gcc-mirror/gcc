#!/bin/sh
# Like mv $1 $2, but if the files are the same, just delete $1.
# Status is 0 if $2 is changed, 1 otherwise.
if
test -r $2
then
if
cmp -s $1 $2
then
echo $2 is unchanged
rm -f $1
else
mv -f $1 $2
fi
else
mv -f $1 $2
fi
