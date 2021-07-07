/* m2range.h header file for M2Range.mod.

Copyright (C) 2012-2021 Free Software Foundation, Inc.
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

#if !defined(m2range_h)
#define m2range_h
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */

EXTERN tree M2Range_BuildIfCallWholeHandlerLoc (location_t location,
                                                tree condition,
						const char *scope,
                                                const char *message);
EXTERN tree M2Range_BuildIfCallRealHandlerLoc (location_t location,
                                               tree condition,
					       const char *scope,
                                               const char *message);

#undef EXTERN
#endif /* m2range_h.  */
