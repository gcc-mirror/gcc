/* Copyright (C) 2007 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */


/* The free tag table used by the MFC tag manager, with tag0
   reserved for the overlay manager.  */
__vector unsigned int
__mfc_tag_table = (__vector unsigned int) { 0x7FFFFFFF, -1, -1, -1 };

/* Arrange to release tag0 if overlays are not present.  */
static void __mfc_tag_init (void) __attribute__ ((constructor));

static void
__mfc_tag_init (void)
{
  extern void _ovly_table __attribute__ ((weak));

  if (&_ovly_table == 0)
    __mfc_tag_table = (__vector unsigned int) { -1, -1, -1, -1 };
}
