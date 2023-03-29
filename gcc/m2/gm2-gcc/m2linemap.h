/* m2linemap.h header file for m2linemap.cc.

Copyright (C) 2012-2023 Free Software Foundation, Inc.
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

#if !defined(m2linemap_h)

#include "input.h"

#define m2linemap_h
#if defined(m2linemap_c)
#if (__cplusplus)
#define EXTERN extern "C"
#else /* !__cplusplus.  */
#define EXTERN
#endif /*!__cplusplus.  */
#else /* !m2linemap_c.  */
#if (__cplusplus)
#define EXTERN extern "C"
#else /* !__cplusplus.  */
#define EXTERN extern
#endif /* !__cplusplus.  */
#endif /* !m2linemap_c.  */

EXTERN void m2linemap_StartFile (void *filename, unsigned int linebegin);
EXTERN void m2linemap_EndFile (void);
EXTERN void m2linemap_StartLine (unsigned int linenumber,
                                 unsigned int linesize);
EXTERN location_t m2linemap_GetLocationColumn (unsigned int column);
EXTERN location_t m2linemap_GetLocationRange (unsigned int start, unsigned int end);
EXTERN location_t m2linemap_GetLocationBinary (location_t caret,
					       location_t start, location_t finish);

EXTERN location_t m2linemap_UnknownLocation (void);
EXTERN location_t m2linemap_BuiltinsLocation (void);

EXTERN location_t m2linemap_GetLocationColumn (unsigned int column);
EXTERN int m2linemap_GetLineNoFromLocation (location_t location);
EXTERN int m2linemap_GetColumnNoFromLocation (location_t location);
EXTERN const char *m2linemap_GetFilenameFromLocation (location_t location);
EXTERN void m2linemap_ErrorAt (location_t location, char *message);
EXTERN void m2linemap_ErrorAtf (location_t location, const char *message);
EXTERN void m2linemap_WarningAtf (location_t location, const char *message);
EXTERN void m2linemap_NoteAtf (location_t location, const char *message);
EXTERN void m2linemap_internal_error (const char *message);
EXTERN void m2linemap_internal_error_at (location_t location, const char *fmt, ...);

EXTERN location_t UnknownLocation (void);
EXTERN location_t BuiltinsLocation (void);
EXTERN void ErrorAt (location_t location, char *message);
EXTERN void ErrorAtf (location_t location, const char *message, ...);
EXTERN void WarningAtf (location_t location, const char *message, ...);
EXTERN void NoteAtf (location_t location, const char *message, ...);

#undef EXTERN
#endif /* m2linemap_h.  */
