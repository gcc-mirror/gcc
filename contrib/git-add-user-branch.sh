#! /bin/sh -e

# Create a new upstream user branch.

# Usage:
#  contrib/git-add-user-branch.sh [<personal-prefix>/]<branch-name> <base>

usage ()
{
    echo "Usage:"
    echo "  $0 [<personal-prefix>/]<branch-name> <start-point>"
    echo
    echo "personal space must already have been set up using"
    echo "contrib/gcc-git-customization.sh"
    exit 1
}

if [ $# != 2 ]
then
    usage
fi

userpfx=$(git config --get "gcc-config.userpfx")
user=$(git config --get "gcc-config.user")

if [ -z "$userpfx" -o -z "$user" ]
then
    usage
fi

branch=$(echo "$1" | sed -r "s:(${userpfx}/)?(.*)$:\2:")
start=$2

# Sanity check the new branch argument.  If there is no '/', then the
# vendor will be the same as the entire first argument.
if [ -z "$branch" ]
then
    usage
fi

git push users/${userpfx} ${start}:refs/users/${user}/heads/${branch}
git fetch -q users/${userpfx}
git branch ${userpfx}/${branch} remotes/users/${userpfx}/${branch}
echo "You are now ready to check out ${userpfx}/${branch}"
echo "To push the branch upstream use:"
echo "  git push users/${userpfx} ${userpfx}/${branch}"
