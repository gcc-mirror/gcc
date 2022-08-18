#!/bin/sh

usage ()
{
    echo "Usage: $0 [--enable-push] <vendor>"
    echo "The following vendors are already known:"
    git ls-remote ${upstream} "*/vendors/*" | sed -r "s:.*/vendors/([^/]+)/.*:\1:"|sort|uniq
    exit 1
}

# Should we insert a "push" refspec to enable pushing to the vendor branch?
enable_push=no

upstream=`git config --get "gcc-config.upstream"`
if [ x"$upstream" = x ]
then
    echo "Config gcc-config.upstream not set, run contrib/gcc-git-customization.sh"
    exit 1
fi

case $# in
    1)
	# vendor names never start with -, so catch this in case user wrote something like --help.
	case "$1" in
	    -*)
		usage
		;;
	    *)
		vendor=$1
		;;
	esac
	;;
    2)
	vendor=$2
	if [ "$1" = "--enable-push" ]
	then
	    enable_push=yes
	else
	    usage
	fi
	;;
    *)
	usage
	;;
esac


echo "setting up git to fetch vendor ${vendor} to remotes/vendors/${vendor}"
url=$(git config --get "remote.${upstream}.url")
pushurl=$(git config --get "remote.${upstream}.pushurl")
git config "remote.vendors/${vendor}.url" "${url}"
if [ "x$pushurl" != "x" ]
then
    git config "remote.vendors/${vendor}.pushurl" "${pushurl}"
fi
git config --replace-all "remote.vendors/${vendor}.fetch" "+refs/vendors/${vendor}/heads/*:refs/remotes/vendors/${vendor}/*" "refs/vendors/${vendor}/heads"
git config --replace-all "remote.vendors/${vendor}.fetch" "+refs/vendors/${vendor}/tags/*:refs/tags/vendors/${vendor}/*" "refs/vendors/${vendor}/tags"
if [ "$enable_push" = "yes" ]
then
    echo "Warning: take care when pushing that you only push the changes you intend."
    echo "E.g. use \"git push vendors/${vendor} HEAD\" to push the current branch"
    git config --replace-all "remote.vendors/${vendor}.push" "refs/heads/${vendor}/*:refs/vendors/${vendor}/heads/*"
else
    git config --unset-all "remote.vendors/${vendor}.push"
fi
git fetch vendors/${vendor}
