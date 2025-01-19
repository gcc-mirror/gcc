#!/bin/sh
#set -x

# Prepares a patch for the patch tester.
# Copyright (C) 2007-2025 Free Software Foundation, Inc.
# Contributed by Sebastian Pop <sebastian.pop@amd.com>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

usage() {
    cat <<EOF
prepare_patch.sh <source_dir> [patches_dir]

    SOURCE_DIR is the directory containing GCC's toplevel configure.

    PATCHES_DIR is the directory where the patch will be copied to.
    Default is SOURCE_DIR/patches.

EOF
    exit 1
}

test $# -eq 0 && usage

SOURCE=$1
PATCHES=

if [[ "$#" < 2 ]]; then
    PATCHES=$SOURCE/patches
else
    PATCHES=$2
fi

[ -f $SOURCE/config.guess ] || usage
[ -d $PATCHES ] || mkdir -p $PATCHES

echo "Enter a name for this patch: "
read name
PATCH=$PATCHES/`TZ=UTC date +"%Y_%m_%d_%H_%M_%S"`_$name.diff

echo "Enter the email where the report should be sent: "
read email
echo "email:$email" >> $PATCH

branch=`svn info $SOURCE | grep URL: | sed -e "s/^URL: //g"`
echo "Enter svn branch (svn info in $SOURCE reports $branch, default is trunk): "
read svn_branch
if [ x$svn_branch = x ]; then
    svn_branch=trunk
fi
echo "branch:$svn_branch" >> $PATCH

revision=`svn info $SOURCE | grep Revision: | sed -e "s/^Revision: //g"`
echo "Enter svn revision (svn info in $SOURCE reports $revision, default is HEAD): "
read svn_revision
if [ x$svn_revision = x ]; then
    svn_revision=HEAD
fi
echo "revision:$svn_revision" >> $PATCH

echo "Enter configure options: "
read configure_options
echo "configure:$configure_options" >> $PATCH

echo "Enter make options: "
read make_options
echo "make:$make_options" >> $PATCH

echo "Enter make check options: "
read check_options
echo "check:$check_options" >> $PATCH

echo "" >> $PATCH

svn diff $SOURCE | tee -a $PATCH

cat <<EOF

You can now edit your patch, include a ChangeLog, and before
submitting to the patch tester, don't forget to sign it with:

  gpg --clearsign $PATCH

EOF
