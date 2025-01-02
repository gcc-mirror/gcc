# Generate gcn-tables.opt from gcn-devices.def
#
# Copyright (C) 2024-2025 Free Software Foundation, Inc.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

BEGIN {
  FS= "[(,] *"

  print "; -*- buffer-read-only: t -*-"
  print "; Generated automatically by gen-opt-tables.awk from gcn-devices.def."
  print "; Do not edit."
  print ""
  print "; Copyright (C) 2024-2025 Free Software Foundation, Inc."
  print ""
  print "; This file is part of GCC."
  print ""
  print "; GCC is free software; you can redistribute it and/or modify"
  print "; it under the terms of the GNU General Public License as"
  print "; published by the Free Software Foundation; either version 3,"
  print "; or (at your option) any later version."
  print ""
  print "; GCC is distributed in the hope that it will be useful,"
  print "; but WITHOUT ANY WARRANTY; without even the implied warranty of"
  print "; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
  print "; GNU General Public License for more details."
  print ""
  print "; You should have received a copy of the GNU General Public"
  print "; License along with GCC; see the file COPYING3.  If not see"
  print "; <http://www.gnu.org/licenses/>."
  print ""
  print "Enum"
  print "Name(gpu_type) Type(enum processor_type)"
  print "GCN GPU type to use:"
}

/^GCN_DEVICE\(/ {
  print ""
  print "EnumValue"
  print "Enum(gpu_type) String(" $2 ") Value(PROCESSOR_" $3 ")"
}
