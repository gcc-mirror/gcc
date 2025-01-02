/* m2color.h interface to gcc colorization.

Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

#if !defined(m2color_h)
#define m2color_h
#if defined(m2color_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else /* !m2color_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */
#endif /* !m2color_c.  */


EXTERN char *
m2color_colorize_start (bool show_color, void *name, unsigned int name_len);

EXTERN char *m2color_colorize_stop (bool show_color);

EXTERN char *m2color_open_quote (void);

EXTERN char *m2color_close_quote (void);

EXTERN void _M2_m2color_init ();
EXTERN void _M2_m2color_finish ();


#endif
