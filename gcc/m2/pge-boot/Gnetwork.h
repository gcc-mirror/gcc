/* Gnetwork.h provides prototypes to htonl and htons.

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

#if !defined(_network_H)
#define _network_H

#ifdef __cplusplus
extern "C" {
#endif
#if !defined(PROC_D)
#define PROC_D
typedef void (*PROC_t) (void);
typedef struct
{
  PROC_t proc;
} PROC;
#endif

#if defined(_network_C)
#define EXTERN
#else
#define EXTERN extern
#endif

/* htons returns a network ordered SHORTCARD.  */

EXTERN short unsigned int network_htons (short unsigned int s);

/* htonl returns a network ordered CARDINAL.  */

EXTERN unsigned int network_htonl (unsigned int s);

#ifdef __cplusplus
}
#endif

#undef EXTERN
#endif
