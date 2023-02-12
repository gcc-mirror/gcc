#!/usr/bin/awk -f
# Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

# This awk script checks that aarch64-sve.md (passed either on the
# command line or via stdin) has an up-to-date contents section.

BEGIN {
  seen1 = 0
  seen2 = 0
  errors = 0
}

# The headings in the comments use a two-level hierarchy: ";; == ..."
# for major sections and ";; ---- ..." for minor sections.  Each section
# heading must be unique.
#
# The contents section should list all the section headings, using the
# same text and in the same order.  We should therefore see exactly two
# copies of the section list.
/^;; == / || /^;; ---- / {
  if ($0 in seen || seen2 > 0)
    {
      if (seen2 >= seen1)
	{
	  printf "error: line not in contents: %s\n", $0 > "/dev/stderr"
	  errors += 1
	  exit(1)
	}
      if ($0 != order[seen2])
	{
	  printf "error: mismatched contents\n     saw: %s\nexpected: %s\n", \
	    $0, order[seen2] > "/dev/stderr"
	  errors += 1
	  exit(1)
	}
      seen2 += 1
    }
  else
    {
      seen[$0] = 1
      order[seen1] = $0
      seen1 += 1
    }
}

END {
  if (seen2 < seen1 && errors == 0)
    {
      printf "error: line only in contents: %s\n", order[seen2] > "/dev/stderr"
      exit(1)
    }
}
