/* Debug hooks for GCC.
   Copyright (C) 2001 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef GCC_DEBUG_H
#define GCC_DEBUG_H

/* This structure contains hooks for the debug information output
   functions, accessed through the global instance debug_hooks set in
   toplev.c according to command line options.  */
struct gcc_debug_hooks
{
  /* Initialise debug output to FILE.  MAIN_FILENAME is the name of
     the main input file.  */
  void (* init) PARAMS ((FILE * file, const char *main_filename));

  /* Output debug symbols to FILE.  */
  void (* finish) PARAMS ((FILE * file, const char *main_filename));
};

extern struct gcc_debug_hooks *debug_hooks;

/* The do-nothing hooks.  */
extern void debug_nothing_init_finish
  PARAMS ((FILE *, const char *));

extern struct gcc_debug_hooks do_nothing_debug_hooks;
extern struct gcc_debug_hooks dbx_debug_hooks;
extern struct gcc_debug_hooks sdb_debug_hooks;
extern struct gcc_debug_hooks dwarf_debug_hooks;
extern struct gcc_debug_hooks dwarf2_debug_hooks;

#endif /* !GCC_DEBUG_H  */
