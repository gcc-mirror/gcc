/* Implementation of file prefix remapping support (-f*-prefix-map options).
   Copyright (C) 2017-2021 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "file-prefix-map.h"

/* Structure recording the mapping from source file and directory names at
   compile time to those to be embedded in the compilation result (debug
   information, the __FILE__ macro expansion, etc).  */
struct file_prefix_map
{
  const char *old_prefix;
  const char *new_prefix;
  size_t old_len;
  size_t new_len;
  struct file_prefix_map *next;
};

/* Record a file prefix mapping in the specified map.  ARG is the argument to
   -f*-prefix-map and must be of the form OLD=NEW.  OPT is the option name
   for diagnostics.  */
static void
add_prefix_map (file_prefix_map *&maps, const char *arg, const char *opt)
{
  file_prefix_map *map;
  const char *p;

  /* Note: looking for the last '='. The thinking is we can control the paths
     inside our projects but not where the users build them.  */
  p = strrchr (arg, '=');
  if (!p)
    {
      error ("invalid argument %qs to %qs", arg, opt);
      return;
    }
  map = XNEW (file_prefix_map);
  map->old_prefix = xstrndup (arg, p - arg);
  map->old_len = p - arg;
  p++;
  map->new_prefix = xstrdup (p);
  map->new_len = strlen (p);
  map->next = maps;
  maps = map;
}

/* Perform user-specified mapping of filename prefixes.  Return the
   GC-allocated new name corresponding to FILENAME or FILENAME if no
   remapping was performed.  */

static const char *
remap_filename (file_prefix_map *maps, const char *filename)
{
  file_prefix_map *map;
  char *s;
  const char *name;
  size_t name_len;

  for (map = maps; map; map = map->next)
    if (filename_ncmp (filename, map->old_prefix, map->old_len) == 0)
      break;
  if (!map)
    return filename;
  name = filename + map->old_len;
  name_len = strlen (name) + 1;

  s = (char *) ggc_alloc_atomic (name_len + map->new_len);
  memcpy (s, map->new_prefix, map->new_len);
  memcpy (s + map->new_len, name, name_len);
  return s;
}

/* NOTE: if adding another -f*-prefix-map option then don't forget to
   ignore it in DW_AT_producer (dwarf2out.c).  */

/* Linked lists of file_prefix_map structures.  */
static file_prefix_map *macro_prefix_maps; /* -fmacro-prefix-map  */
static file_prefix_map *debug_prefix_maps; /* -fdebug-prefix-map  */

/* Record a file prefix mapping for -fmacro-prefix-map.  */
void
add_macro_prefix_map (const char *arg)
{
  add_prefix_map (macro_prefix_maps, arg, "-fmacro-prefix-map");
}

/* Record a file prefix mapping for -fdebug-prefix-map.  */
void
add_debug_prefix_map (const char *arg)
{
  add_prefix_map (debug_prefix_maps, arg, "-fdebug-prefix-map");
}

/* Record a file prefix mapping for all -f*-prefix-map.  */
void
add_file_prefix_map (const char *arg)
{
  add_prefix_map (macro_prefix_maps, arg, "-ffile-prefix-map");
  add_prefix_map (debug_prefix_maps, arg, "-ffile-prefix-map");
}

/* Remap using -fmacro-prefix-map.  Return the GC-allocated new name
   corresponding to FILENAME or FILENAME if no remapping was performed.  */
const char *
remap_macro_filename (const char *filename)
{
  return remap_filename (macro_prefix_maps, filename);
}

/* Remap using -fdebug-prefix-map.  Return the GC-allocated new name
   corresponding to FILENAME or FILENAME if no remapping was performed.  */
const char *
remap_debug_filename (const char *filename)
{
  return remap_filename (debug_prefix_maps, filename);
}
