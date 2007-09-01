/* Operating system specific defines to be used when targeting GCC for any
   Solaris 2 system up to Solaris 2.6.
   Copyright 2004, 2007 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC "\
%{pthreads|pthread:-D_REENTRANT -D_PTHREADS95} \
%{!pthreads:%{!pthread:%{threads:-D_REENTRANT -D_SOLARIS_THREADS}}} \
%{compat-bsd:-iwithprefixbefore ucbinclude -I/usr/ucbinclude} \
"
