#!/bin/sh

# Script to add some local git customizations suitable for working
# with the GCC git repository

ask () {
    question=$1
    default=$2
    var=$3
    echo -n $question "["$default"]? "
    read answer
    if [ "x$answer" = "x" ]
    then
	eval $var=$default
    else
	eval $var=$answer
    fi
}

# Add a git command to find the git commit equivalent to legacy SVN revision NNN
git config alias.svn-rev '!f() { rev=$1; shift; git log --all --grep="From-SVN: r\\?$rev\\b" "${@}"; } ; f'

# Make diff on MD files uses "(define" as a function marker.
# Use this in conjunction with a .gitattributes file containing
# *.md    diff=md
git config diff.md.xfuncname '^\(define.*$'

upstream=`git config --get "gcc-config.upstream"`
if [ "x$upstream" = "x" ]
then
    upstream="origin"
fi
ask "Local name for upstream repository" "origin" upstream
git config "gcc-config.upstream" "$upstream"

remote_id=`git config --get "gcc-config.user"`
if [ "x$remote_id" = "x" ]
then
    # See if the url specifies the remote user name.
    url=`git config --get "remote.$upstream.url"`
    if [ "x$url" = "x" ]
    then
	# This is a pure guess, but for many people it might be OK.
	remote_id=`whoami`
    else
	remote_id=`echo $url | sed -r "s|^.*ssh://(.+)@gcc.gnu.org.*$|\1|"`
	if [ x$remote_id = x$url ]
	then
	    remote_id=`whoami`
	fi
    fi
fi
ask "Account name on gcc.gnu.org" $remote_id remote_id
git config "gcc-config.user" "$remote_id"

old_pfx=`git config --get "gcc-config.userpfx"`
if [ "x$old_pfx" = "x" ]
then
    old_pfx="me"
fi
echo "Local branch prefix for personal branches you want to share"
echo "(local branches starting <prefix>/ can be pushed directly to your"
ask "personal area on the gcc server)" $old_pfx new_pfx
git config "gcc-config.userpfx" "$new_pfx"

echo "Setting up tracking for personal namespace $remote_id in remotes/$upstream/${new_pfx}"
git config --replace-all "remote.${upstream}.fetch" "+refs/users/${remote_id}/heads/*:refs/remotes/${upstream}/${new_pfx}/*" ":refs/remotes/${upstream}/${old_pfx}/"
git config --replace-all "remote.${upstream}.fetch" "+refs/users/${remote_id}/tags/*:refs/tags/${new_pfx}/*" ":refs/tags/${old_pfx}/"
git config --replace-all "remote.${upstream}.push" "refs/heads/${new_pfx}/*:refs/users/${remote_id}/heads/*" "^\+?refs/heads/${old_pfx}/"
