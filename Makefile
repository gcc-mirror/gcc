#
# Redirecting Makefile.
#   Copyright (C) 2002 Free Software Foundation
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


# This Makefile is a red herring.  It is not normally used and it is never
# written by 'configure' from 'Makefile.in'.  This Makefile is here to
# support users who try to configure/build in the source directory.  It
# simply chdir's into a subdirectory created by configure and reinvokes
# make.  Few targets are listed because advanced users (who would use those
# targets) are assumed to know that a separate build dir is recommended.

SHELL = /bin/sh

# Additional pass-through targets can be listed here.
TARGETS=bootstrap \
        clean \
        install

all:
.PHONY: sanitycheck

distclean:
	test -f s-buildd || exit 0 &&  \
	  . ./s-buildd && rm -rf $${builddir} s-buildd

sanitycheck:
	@if test ! -f s-buildd; then  \
	  echo 'You must configure before attempting to build.';  \
	  echo 'Please read the instructions in the INSTALL directory.';  \
	  exit 1;  \
	fi

$(TARGETS): sanitycheck
	(. ./s-buildd; cd $${builddir}; ${MAKE} $@)

all: sanitycheck
	(. ./s-buildd; cd $${builddir}; ${MAKE} $${defaulttarget})

