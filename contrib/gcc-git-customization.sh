#!/bin/sh

# Script to add some local git customizations suitable for working
# with the GCC git repository

ask () {
    question=$1
    default=$2
    var=$3
    printf "%s [%s]? " "$question" "$default"
    read answer
    if [ "x$answer" = "x" ]
    then
	eval $var=\$default
    else
	eval $var=\$answer
    fi
}

# Add a git command to find the git commit equivalent to legacy SVN revision NNN
git config alias.svn-rev '!f() { rev=$1; shift; git log --all --grep="^From-SVN: r\\?$rev\\b" "${@}"; } ; f'

# Add git commands to convert git commit to monotonically increasing revision number
# and vice versa
git config alias.gcc-descr '!f() { "`git rev-parse --show-toplevel`/contrib/git-descr.sh" $@; } ; f'
git config alias.gcc-undescr '!f() { "`git rev-parse --show-toplevel`/contrib/git-undescr.sh" $@; } ; f'

git config alias.gcc-verify '!f() { "`git rev-parse --show-toplevel`/contrib/gcc-changelog/git_check_commit.py" $@; } ; f'
git config alias.gcc-backport '!f() { "`git rev-parse --show-toplevel`/contrib/git-backport.py" $@; } ; f'
git config alias.gcc-fix-changelog '!f() { "`git rev-parse --show-toplevel`/contrib/git-fix-changelog.py" $@; } ; f'
git config alias.gcc-mklog '!f() { "`git rev-parse --show-toplevel`/contrib/mklog.py" $@; } ; f'
git config alias.gcc-commit-mklog '!f() { "`git rev-parse --show-toplevel`/contrib/git-commit-mklog.py" "$@"; }; f'
git config alias.gcc-style '!f() {
    check=`git rev-parse --show-toplevel`/contrib/check_GNU_style.py;
    arg=; if [ $# -ge 1 ] && [ "$1" != "-f" ]; then arg="$1"; shift;
    elif [ $# -eq 3 ]; then arg="$3"; set -- "$1" "$2"; fi
    git show $arg | $check "$@" -; }; f'

# Make diff on MD files use "(define" as a function marker.
# Use this in conjunction with a .gitattributes file containing
# *.md    diff=md
git config diff.md.xfuncname '^\(define.*$'

# Tell git send-email where patches go.
# ??? Maybe also set sendemail.tocmd to guess from MAINTAINERS?
git config sendemail.to 'gcc-patches@gcc.gnu.org'

set_user=$(git config --get "user.name")
set_email=$(git config --get "user.email")

if [ "x$set_user" = "x" ]
then
    # Try to guess the user's name by looking it up in the password file
    if type getent >/dev/null 2>&1; then
      new_user=$(getent passwd $(whoami) | awk -F: '{ print $5 }')
    elif [ $(uname -s) = Darwin ]; then
      new_user=$(id -F 2>/dev/null)
    fi
    if [ "x$new_user" = "x" ]
    then
       new_user="(no default)"
    fi
else
    new_user=$set_user
fi
ask "Your name" "${new_user}" new_user
if [ "x$new_user" = "x(no default)" ]
then
    echo "Cannot continue, git needs to record your name against commits"
    exit 1
fi

if [ "x$set_email" = "x" ]
then
    new_email="(no_default)"
else
    new_email=$set_email
fi

ask "Your email address (for git commits)" "${new_email}" new_email
if [ "x$new_email" = "x(no default)" ]
then
    echo "Cannot continue, git needs to record your email address against commits"
    exit 1
fi

if [ "x$set_user" != "x$new_user" ]
then
    git config "user.name" "$new_user"
fi

if [ "x$set_email" != "x$new_email" ]
then
    git config "user.email" "$new_email"
fi

upstream=$(git config --get "gcc-config.upstream")
if [ "x$upstream" = "x" ]
then
    upstream="origin"
fi
ask "Local name for upstream repository" "origin" upstream

v=$(git config --get-all "remote.${upstream}.fetch")
if [ "x$v" = "x" ]
then
    echo "Remote $upstream does not seem to exist as a remote"
    exit 1
fi
git config "gcc-config.upstream" "$upstream"

remote_id=$(git config --get "gcc-config.user")
if [ "x$remote_id" = "x" ]
then
    # See if the url specifies the remote user name.
    url=$(git config --get "remote.$upstream.url")
    if [ "x$url" = "x" ]
    then
	# This is a pure guess, but for many people it might be OK.
	remote_id=$(whoami)
    else
	remote_id=$(echo $url | sed 's|^.*ssh://\(..*\)@gcc.gnu.org.*$|\1|')
	if [ x$remote_id = x$url ]
	then
	    remote_id=$(whoami)
	fi
    fi
fi

ask "Account name on gcc.gnu.org (for your personal branches area)" "$remote_id" remote_id
git config "gcc-config.user" "$remote_id"

old_pfx=$(git config --get "gcc-config.userpfx")
if [ "x$old_pfx" = "x" ]
then
    old_pfx="me"
fi
echo
echo "Local branch prefix for personal branches you want to share"
echo "(local branches starting <prefix>/ can be pushed directly to your"
ask "personal area on the gcc server)" $old_pfx new_pfx
git config "gcc-config.userpfx" "$new_pfx"

echo
ask "Install prepare-commit-msg git hook for 'git commit-mklog' alias" yes dohook
if [ "x$dohook" = xyes ]; then
    hookdir=`git rev-parse --git-path hooks 2>/dev/null`
    if [ $? -eq 0 ]; then
	if [ -f "$hookdir/prepare-commit-msg" ]; then
	    echo " Moving existing prepare-commit-msg hook to prepare-commit-msg.bak"
	    mv "$hookdir/prepare-commit-msg" "$hookdir/prepare-commit-msg.bak"
	fi
	install -c "`git rev-parse --show-toplevel`/contrib/prepare-commit-msg" "$hookdir"
    else
	echo " `git --version` is too old, cannot find hooks dir"
    fi
fi

# Scan the existing settings to see if there are any we need to rewrite.
vendors=$(git config --get-all "remote.${upstream}.fetch" "refs/vendors/" | sed 's:.*refs/vendors/\([^/][^/]*\)/.*:\1:' | sort | uniq)
url=$(git config --get "remote.${upstream}.url")
pushurl=$(git config --get "remote.${upstream}.pushurl")
for v in $vendors
do
    echo "Migrating vendor \"$v\" to new remote \"vendors/$v\""
    git config --unset-all "remote.${upstream}.fetch" "refs/vendors/$v/"
    git config --unset-all "remote.${upstream}.push" "refs/vendors/$v/"
    git config "remote.vendors/${v}.url" "${url}"
    if [ "x$pushurl" != "x" ]
    then
	git config "remote.vendors/${v}.pushurl" "${pushurl}"
    fi
    git config --add "remote.vendors/${v}.fetch" "+refs/vendors/$v/heads/*:refs/remotes/vendors/${v}/*"
    git config --add "remote.vendors/${v}.fetch" "+refs/vendors/$v/tags/*:refs/tags/vendors/${v}/*"
done

# Convert the remote 'pfx' to users/pfx to avoid problems with ambiguous refs
# on user branches
old_remote=$(git config --get "remote.${old_pfx}.url")
if [ -n "${old_remote}" ]
then
    echo "Migrating remote \"${old_pfx}\" to new remote \"users/${new_pfx}\""
    # Create a dummy fetch rule that will cause the subsequent prune to remove the old remote refs.
    git config --replace-all "remote.${old_pfx}.fetch" "+refs/empty/*:refs/remotes/${old_pfx}/*"
    # Remove any remotes
    git remote prune ${old_pfx}
    git config --remove-section "remote.${old_pfx}"
    for br in $(git branch --list "${old_pfx}/*")
    do
	old_remote=$(git config --get "branch.${br}.remote")
	if [ "${old_remote}" = "${old_pfx}" ]
	then
	    git config "branch.${br}.remote" "users/${new_pfx}"
	fi
    done
fi

echo "Setting up tracking for personal namespace $remote_id in remotes/users/${new_pfx}"
git config "remote.users/${new_pfx}.url" "${url}"
if [ "x$pushurl" != "x" ]
then
    git config "remote.users/${new_pfx}.pushurl" "${pushurl}"
fi
git config --replace-all "remote.users/${new_pfx}.fetch" "+refs/users/${remote_id}/heads/*:refs/remotes/users/${new_pfx}/*" "refs/users/${remote_id}/heads/"
git config --replace-all "remote.users/${new_pfx}.fetch" "+refs/users/${remote_id}/tags/*:refs/tags/users/${new_pfx}/*" "refs/users/${remote_id}/tags/"
git config --replace-all "remote.users/${new_pfx}.push" "refs/heads/${new_pfx}/*:refs/users/${remote_id}/heads/*" "refs/users/${remote_id}"

if [ "$old_pfx" != "$new_pfx" -a "$old_pfx" != "${upstream}" ]
then
    git config --remove-section "remote.${old_pfx}"
fi

git config --unset-all "remote.${upstream}.fetch" "refs/users/${remote_id}/"
git config --unset-all "remote.${upstream}.push" "refs/users/${remote_id}/"

git fetch "users/${new_pfx}"
