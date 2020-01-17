#!/bin/sh

if [ $# != 1 ]
then
    echo "Usage: $0 <vendor>"
    exit 1
fi

vendor=$1
upstream=`git config --get "gcc-config.upstream"`
if [ x"$upstream" = x ]
then
    echo "Config gcc-config.upstream not set, run contrib/gcc-git-customization"
    exit 1
fi

echo "setting up git to fetch vendor ${vendor} to remotes/${upstream}/${vendor}"
git config --replace-all "remote.${upstream}.fetch" "+refs/vendors/${vendor}/heads/*:refs/remotes/${upstream}/${vendor}/*" ":refs/remotes/${upstream}/${vendor}/"
git config --replace-all "remote.${upstream}.fetch" "+refs/vendors/${vendor}/tags/*:refs/tags/${vendor}/*" ":refs/tags/${vendor}/"
git fetch
