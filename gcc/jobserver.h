/* GNU Jobserver Integration Interface.
   Copyright (C) 2005-2020 Free Software Foundation, Inc.

   Contributed by Giuliano Belinassi <giuliano.belinassi@usp.br>

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

#define JOBSERVER_NULL_TOKEN ('\0')

typedef char jobserver_token_t;

extern bool jobserver_initialized;
extern jobserver_token_t jobserver_curr_token;

bool jobserver_initialize ();
bool jobserver_finalize ();
jobserver_token_t jobserver_get_token ();
void jobserver_return_token (jobserver_token_t);

