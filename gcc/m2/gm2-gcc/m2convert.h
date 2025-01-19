/* m2convert.h header file for m2convert.cc.

Copyright (C) 2012-2025 Free Software Foundation, Inc.
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

#if !defined(m2convert_h)
#define m2convert_h
#if defined(m2convert_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else /* m2convert_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */
#endif /* m2convert_c.  */

EXTERN tree m2convert_BuildConvert (location_t location, tree type, tree value,
                                    bool checkOverflow);
EXTERN tree m2convert_ConvertToPtr (location_t location_t, tree p);
EXTERN tree m2convert_ConvertString (tree type, tree expr);
EXTERN tree m2convert_ConvertConstantAndCheck (location_t location, tree type,
                                               tree expr);
EXTERN tree m2convert_convertToPtr (location_t location, tree type);
EXTERN tree m2convert_ToCardinal (location_t location, tree expr);
EXTERN tree m2convert_ToInteger (location_t location, tree expr);
EXTERN tree m2convert_ToWord (location_t location, tree expr);
EXTERN tree m2convert_ToBitset (location_t location, tree expr);
EXTERN tree m2convert_ToLoc (location_t location, tree expr);
EXTERN tree m2convert_GenericToType (location_t location, tree type,
                                     tree expr);

#undef EXTERN
#endif /* m2convert_h.  */
