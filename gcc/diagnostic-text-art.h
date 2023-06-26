/* Copyright (C) 2023 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#ifndef GCC_DIAGNOSTIC_TEXT_ART_H
#define GCC_DIAGNOSTIC_TEXT_ART_H

/* Values for -fdiagnostics-text-art-charset=.  */

enum diagnostic_text_art_charset
{
  /* No text art diagrams shall be emitted.  */
  DIAGNOSTICS_TEXT_ART_CHARSET_NONE,

  /* Use pure ASCII for text art diagrams.  */
  DIAGNOSTICS_TEXT_ART_CHARSET_ASCII,

  /* Use ASCII + conservative use of other unicode characters
     in text art diagrams.  */
  DIAGNOSTICS_TEXT_ART_CHARSET_UNICODE,

  /* Use Emoji.  */
  DIAGNOSTICS_TEXT_ART_CHARSET_EMOJI
};

const enum diagnostic_text_art_charset DIAGNOSTICS_TEXT_ART_CHARSET_DEFAULT
  = DIAGNOSTICS_TEXT_ART_CHARSET_EMOJI;

extern void
diagnostics_text_art_charset_init (diagnostic_context *context,
				  enum diagnostic_text_art_charset charset);


#endif /* ! GCC_DIAGNOSTIC_TEXT_ART_H */
