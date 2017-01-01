#  Copyright (C) 2010-2017 Free Software Foundation, Inc.
#  Contributed by Michael Meissner (meissner@linux.vnet.ibm.com)
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# This Awk script reads in the option records and emits the include files
# listed by the HeaderInclude directive.

BEGIN {
	h_next = 0
	c_next = 0
}

(h_next != 0)	  { print "OPTIONS_H_EXTRA += $(srcdir)/" $1; h_next = 0 }
(c_next != 0)	  { print "OPTIONS_C_EXTRA += $(srcdir)/" $1; c_next = 0 }
/^HeaderInclude$/ { h_next = 1; c_next = 0 }
/^SourceInclude$/ { h_next = 0; c_next = 1 }
