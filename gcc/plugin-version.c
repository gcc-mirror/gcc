/* Version information for plugins.
   Copyright (C) 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "system.h"
#include "gcc-plugin.h"
#include "configargs.h"

static char basever[] = BASEVER;
static char datestamp[] = DATESTAMP;
static char devphase[] = DEVPHASE;
static char revision[] = REVISION;

/* FIXME plugins: We should make the version information more precise.
   One way to do is to add a checksum. */

struct plugin_gcc_version plugin_gcc_version = {basever, datestamp, devphase,
						revision,
						configuration_arguments};
