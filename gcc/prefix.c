/* Utility to update paths from internal to external forms.
   Copyright (C) 1997, 1998 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This file contains routines to update a path, both to canonicalize
   the directory format and to handle any prefix translation.

   This file must be compiled with -DPREFIX= to specify the "prefix"
   value used by configure.  If a filename does not begin with this
   prefix, it will not be affected other than by directory canonicalization.

   Each caller of 'update_path' may specify both a filename and
   a translation prefix and consist of the name of the package that contains
   the file ("@GCC", "@BINUTIL", "@GNU", etc).

   If the prefix is not specified, the filename will only undergo
   directory canonicalization.

   If it is specified, the string given by PREFIX will be replaced
   by the specified prefix (with a '@' in front unless the prefix begins
   with a '$') and further translation will be done as follows
   until none of the two conditions below are met:

   1) If the filename begins with '@', the string between the '@' and
   the end of the name or the first '/' or directory separator will
   be considered a "key" and looked up as follows:

   -- If this is a Win32 OS, then the Registry will be examined for
      an entry of "key" in 

      HKEY_LOCAL_MACHINE\SOFTWARE\Free Software Foundation\

      if found, that value will be used.

   -- If not found (or not a Win32 OS), the environment variable
      key_ROOT (the value of "key" concatenated with the constant "_ROOT")
      is tried.  If that fails, then PREFIX (see above) is used.

   2) If the filename begins with a '$', the rest of the string up
   to the end or the first '/' or directory separator will be used
   as an environment variable, whose value will be returned.

   Once all this is done, any '/' will be converted to DIR_SEPARATOR,
   if they are different. 

   NOTE:  using resolve_keyed_path under Win32 requires linking with
   advapi32.dll.  */


#include "config.h"
#include "gansidecl.h"
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#ifdef _WIN32
#include <windows.h>
#endif

static char *get_key_value	PROTO((char *));
static char *translate_name	PROTO((char *));
static char *concat		PVPROTO((char *, ...));
static char *save_string	PROTO((char *, int));

#ifdef _WIN32
static char *lookup_key		PROTO((char *));
static HKEY reg_key = (HKEY) INVALID_HANDLE_VALUE;
#endif

extern char *getenv ();

/* Given KEY, as above, return its value.  */

static char *
get_key_value (key)
     char *key;
{
  char *prefix = 0;

#ifdef _WIN32
  prefix = lookup_key (key);
#endif

  if (prefix == 0)
    prefix = getenv (concat (key, "_ROOT", NULL_PTR));

  return prefix;
}

/* Concatenate a sequence of strings, returning the result.

   This function is based on the one in libiberty.  */

static char *
concat VPROTO((char *first, ...))
{
  register int length;
  register char *newstr;
  register char *end;
  register char *arg;
  va_list args;
#ifndef __STDC__
  char *first;
#endif

  /* First compute the size of the result and get sufficient memory.  */

  VA_START (args, first);
#ifndef __STDC__
  first = va_arg (args, char *);
#endif

  arg = first;
  length = 0;

  while (arg != 0)
    {
      length += strlen (arg);
      arg = va_arg (args, char *);
    }

  newstr = (char *) malloc (length + 1);
  va_end (args);

  /* Now copy the individual pieces to the result string.  */

  VA_START (args, first);
#ifndef __STDC__
  first = va_arg (args, char *);
#endif

  end = newstr;
  arg = first;
  while (arg != 0)
    {
      while (*arg)
	*end++ = *arg++;
      arg = va_arg (args, char *);
    }
  *end = '\000';
  va_end (args);

  return (newstr);
}

/* Return a copy of a string that has been placed in the heap.  */

static char *
save_string (s, len)
     char *s;
     int len;
{
  register char *result = (char *) malloc (len + 1);

  bcopy (s, result, len);
  result[len] = 0;
  return result;
}

#ifdef _WIN32

/* Look up "key" in the registry, as above.  */

static char *
lookup_key (key)
     char *key;
{
  char *dst;
  DWORD size;
  DWORD type;
  LONG res;

  if (reg_key == (HKEY) INVALID_HANDLE_VALUE)
    {
      res = RegOpenKeyExA (HKEY_LOCAL_MACHINE, "SOFTWARE", 0,
			   KEY_READ, &reg_key);

      if (res == ERROR_SUCCESS)
	res = RegOpenKeyExA (reg_key, "Free Software Foundation", 0,
			     KEY_READ, &reg_key);

      if (res != ERROR_SUCCESS)
        {
          reg_key = (HKEY) INVALID_HANDLE_VALUE;
          return 0;
        }
    }

  size = 32;
  dst = (char *) malloc (size);

  res = RegQueryValueExA (reg_key, key, 0, &type, dst, &size);
  if (res == ERROR_MORE_DATA && type == REG_SZ)
    {
      dst = (char *) realloc (dst, size);
      res = RegQueryValueExA (reg_key, key, 0, &type, dst, &size);
    }

  if (type != REG_SZ || res != ERROR_SUCCESS)
    {
      free (dst);
      dst = 0;
    }

  return dst;
}
#endif

/* If NAME starts with a '@' or '$', apply the translation rules above
   and return a new name.  Otherwise, return the given name.  */

static char *
translate_name (name)
     char *name;
{
  char code = name[0];
  char *key, *prefix = 0;
  int keylen;

  if (code != '@' && code != '$')
    return name;

  for (keylen = 0;
       (name[keylen + 1] != 0 && name[keylen + 1] != '/'
#ifdef DIR_SEPARATOR
	&& name[keylen + 1] != DIR_SEPARATOR
#endif
	);
       keylen++)
    ;

  key = alloca (keylen + 1);
  strncpy (key, &name[1], keylen);
  key[keylen] = 0;

  name = &name[keylen + 1];

  if (code == '@')
    prefix = get_key_value (key);
  else
    prefix = getenv (key);

  if (prefix == 0)
    prefix = PREFIX;

  /* Remove any trailing directory separator from what we got.  */
  if (prefix[strlen (prefix) - 1] == '/'
#ifdef DIR_SEPARATOR
      || prefix[strlen (prefix) - 1] == DIR_SEPARATOR
#endif
      )
    {
      prefix = save_string (prefix, strlen (prefix));
      prefix[strlen (prefix) - 1] = 0;
    }

  return concat (prefix, name, NULL_PTR);
}

/* Update PATH using KEY if PATH starts with PREFIX.  */

char *
update_path (path, key)
     char *path;
     char *key;
{
  if (! strncmp (path, PREFIX, strlen (PREFIX)) && key != 0)
    {
      if (key[0] != '$')
	key = concat ("@", key, NULL_PTR);

      path = concat (key, &path[strlen (PREFIX)], NULL_PTR);

      while (path[0] == '@' || path[0] == '$')
	path = translate_name (path);
    }
      
#ifdef DIR_SEPARATOR
  if (DIR_SEPARATOR != '/')
    {
      int i;
      int len = strlen (path);

      path = save_string (path, len);
      for (i = 0; i < len; i++)
	if (path[i] == '/')
	  path[i] = DIR_SEPARATOR;
    }
#endif

  return path;
}
