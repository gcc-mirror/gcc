/* Gmcrts.h provides prototypes to case and return exceptions.

Copyright (C) 2016-2023 Free Software Foundation, Inc.
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

#if !defined(MCRTS_H)
#define MCRTS_H

#ifdef __cplusplus
extern "C" {
#endif

void CaseException (const char *s, unsigned int high, unsigned int lineno);
void ReturnException (const char *s, unsigned int high, unsigned int lineno);
/* void throw (int n);  */

#ifdef __cplusplus
}
#endif

#endif
