#! /bin/sh -e

# Create a new upstream vendor branch.

# Usage:
#  contrib/git-add-vendor-branch.sh <vendor>/<branch-name> <base>

usage ()
{
    echo "Usage:"
    echo "  $0 <vendor>/<branch-name> <start-point>"
    echo
    echo "<vendor> must have already been set up using contrib/git-fetch-vendor.sh"
    exit 1
}

if [ $# != 2 ]
then
    usage
fi

vendor=$(echo "$1" | sed -r "s:([^/]*)/.*$:\1:")
branch=$(echo "$1" | sed -r "s:[^/]*/(.*)$:\1:")
start=$2

# Sanity check the new branch argument.  If there is no '/', then the
# vendor will be the same as the entire first argument.
if [ -z "$vendor" -o -z "$branch" -o ${vendor} = $1 ]
then
    usage
fi

# Check that we know about the vendor
url=$(git config --get "remote.vendors/${vendor}.url"||true)
if [ -z "$url" ]
then
    echo "Cannot locate remote data for vendor ${vendor}.  Have you set it up?"
    exit 1
fi

git branch --no-track ${vendor}/${branch} ${start}
git push vendors/${vendor} ${vendor}/${branch}:refs/vendors/${vendor}/heads/${branch}
git fetch -q vendors/${vendor}
git branch --set-upstream-to=remotes/vendors/${vendor}/${branch} ${vendor}/$branch
echo "You are now ready to check out ${vendor}/${branch}"
echo "To push the branch upstream, use:"
echo
echo "git push vendors/${vendor} ${vendor}/${branch}"
