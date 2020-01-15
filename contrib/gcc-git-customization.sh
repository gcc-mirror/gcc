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

# Add git commands to convert git commit to monotonically increasing revision number
# and vice versa
git config alias.gcc-descr \!"f() { if test \${1:-no} = --full; then r=\$(git describe --all --abbrev=40 --match 'basepoints/gcc-[0-9]*' \${2:-master} | sed -n 's,^\\(tags/\\)\\?basepoints/gcc-,r,p'); expr match \${r:-no} '^r[0-9]\\+\$' >/dev/null && r=\${r}-0-g\$(git rev-parse \${2:-master}); test -n \$r && echo \${r}; else git describe --all --match 'basepoints/gcc-[0-9]*' \${1:-master} | sed -n 's,^\\(tags/\\)\\?basepoints/gcc-\\([0-9]\\+\\)-\\([0-9]\\+\\)-g[0-9a-f]*\$,r\\2-\\3,p;s,^\\(tags/\\)\\?basepoints/gcc-\\([0-9]\\+\\)\$,r\\2-0,p'; fi; }; f"
git config alias.gcc-undescr \!"f() { o=\$(git config --get gcc-config.upstream); r=\$(echo \$1 | sed -n 's,^r\\([0-9]\\+\\)-[0-9]\\+\$,\\1,p'); n=\$(echo \$1 | sed -n 's,^r[0-9]\\+-\\([0-9]\\+\\)\$,\\1,p'); test -z \$r && echo Invalid id \$1 && exit 1; h=\$(git rev-parse --verify --quiet \${o:-origin}/releases/gcc-\$r); test -z \$h && h=\$(git rev-parse --verify --quiet \${o:-origin}/master); p=\$(git describe --all --match 'basepoints/gcc-'\$r \$h | sed -n 's,^\\(tags/\\)\\?basepoints/gcc-[0-9]\\+-\\([0-9]\\+\\)-g[0-9a-f]*\$,\\2,p;s,^\\(tags/\\)\\?basepoints/gcc-[0-9]\\+\$,0,p'); git rev-parse --verify \$h~\$(expr \$p - \$n); }; f"

# Make diff on MD files use "(define" as a function marker.
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
ask "Account name on gcc.gnu.org (for your personal branches area)" $remote_id remote_id
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

push_rule=`git config --get "remote.${upstream}.push"`
if [ "x$push_rule" != "x" ]
then
    echo "***********************************************"
    echo "                  Warning"
    echo "***********************************************"
    echo
    echo "Old versions of this script used to add custom push"
    echo "rules to simplify pushing to personal branches."
    echo "Your configuration contains such rules, but we no-longer"
    echo "recommend doing this."
    echo
    echo "To delete these rules run:"
    echo "  git config --unset-all \"remote.${upstream}.push\""
fi
