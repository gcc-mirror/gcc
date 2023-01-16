/* network.c provide access to htons and htonl.

Copyright (C) 2010-2023 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#define _network_C
#include "Gnetwork.h"

#include "config.h"
#include "system.h"


short unsigned int
network_htons (short unsigned int s)
{
  return htons (s);
}

unsigned int
network_htonl (unsigned int s)
{
  return htonl (s);
}
