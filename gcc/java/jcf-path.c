/* Handle CLASSPATH, -classpath, and path searching.
   Copyright (C) 1998-2014 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Tom Tromey <tromey@cygnus.com>, October 1998.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include <dirent.h>

#include "jcf.h"

#ifndef DIR_UP
#define DIR_UP ".."
#endif



/* Possible flag values.  */
#define FLAG_SYSTEM 1
#define FLAG_ZIP    2

/* We keep linked lists of directory names.  A ``directory'' can be
   either an ordinary directory or a .zip file.  */
struct entry
{
  char *name;
  int flags;
  struct entry *next;
};

static void free_entry (struct entry **);
static void append_entry (struct entry **, struct entry *);
static void add_entry (struct entry **, const char *, int);
static void add_path (struct entry **, const char *, int);

/* We support several different ways to set the class path.

   built-in system directory (only libgcj.jar)
   CLASSPATH environment variable
   -classpath option overrides $CLASSPATH
   -CLASSPATH option is a synonym for -classpath (for compatibility)
   -bootclasspath overrides built-in
   -extdirs sets the extensions directory path (overrides built-in)
   -I prepends path to list

   We implement this by keeping several path lists, and then simply
   ignoring the ones which are not relevant.  */

/* This holds all the -I directories.  */
static struct entry *include_dirs;

/* This holds the CLASSPATH environment variable.  */
static struct entry *classpath_env;

/* This holds the -classpath command-line option.  */
static struct entry *classpath_user;

/* This holds the default directories.  Some of these will have the
   "system" flag set.  */
static struct entry *sys_dirs;

/* This holds the extensions path entries.  */
static struct entry *extensions;

/* This is the sealed list.  It is just a combination of other lists.  */
static struct entry *sealed;

/* We keep track of the longest path we've seen.  */
static int longest_path = 0;



static void
free_entry (struct entry **entp)
{
  struct entry *e, *n;

  for (e = *entp; e; e = n)
    {
      n = e->next;
      free (e->name);
      free (e);
    }
  *entp = NULL;
}

static void
append_entry (struct entry **entp, struct entry *ent)
{
  /* It doesn't matter if this is slow, since it is run only at
     startup, and then infrequently.  */
  struct entry *e;

  /* Find end of list.  */
  for (e = *entp; e && e->next; e = e->next)
    ;

  if (e)
    e->next = ent;
  else
    *entp = ent;
}

static void
add_entry (struct entry **entp, const char *filename, int is_system)
{
  int len;
  struct entry *n;

  n = XNEW (struct entry);
  n->flags = is_system ? FLAG_SYSTEM : 0;
  n->next = NULL;

  len = strlen (filename);

  if (len > 4 && (FILENAME_CMP (filename + len - 4, ".zip") == 0
		  || FILENAME_CMP (filename + len - 4, ".jar") == 0))
    {
      n->flags |= FLAG_ZIP;
      /* If the user uses -classpath then he'll have to include
	 libgcj.jar in the value.  We check for this in a simplistic
	 way.  Symlinks will fool this test.  This is only used for
	 -MM and -MMD, so it probably isn't terribly important.  */
      if (! FILENAME_CMP (filename, LIBGCJ_ZIP_FILE))
	n->flags |= FLAG_SYSTEM;
    }

  /* Note that we add a trailing separator to `.zip' names as well.
     This is a little hack that lets the searching code in jcf-io.c
     work more easily.  Eww.  */
  if (! IS_DIR_SEPARATOR (filename[len - 1]))
    {
      char *f2 = (char *) alloca (len + 2);
      strcpy (f2, filename);
      f2[len] = DIR_SEPARATOR;
      f2[len + 1] = '\0';
      n->name = xstrdup (f2);
      ++len;
    }
  else
    n->name = xstrdup (filename);

  if (len > longest_path)
    longest_path = len;

  append_entry (entp, n);
}

static void
add_path (struct entry **entp, const char *cp, int is_system)
{
  const char *startp, *endp;

  if (cp)
    {
      char *buf = (char *) alloca (strlen (cp) + 3);
      startp = endp = cp;
      while (1)
	{
	  if (! *endp || *endp == PATH_SEPARATOR)
	    {
	      if (endp == startp)
		{
		  buf[0] = '.';
		  buf[1] = DIR_SEPARATOR;
		  buf[2] = '\0';
		}
	      else
		{
		  strncpy (buf, startp, endp - startp);
		  buf[endp - startp] = '\0';
		}
	      add_entry (entp, buf, is_system);
	      if (! *endp)
		break;
	      ++endp;
	      startp = endp;
	    }
	  else
	    ++endp;
	}
    }
}

static int init_done = 0;

/* Initialize the path module.  */
void
jcf_path_init (void)
{
  char *cp;
  char *attempt, sep[2];
  struct stat stat_b;
  int found = 0, len;

  if (init_done)
    return;
  init_done = 1;

  sep[0] = DIR_SEPARATOR;
  sep[1] = '\0';

  cp = getenv ("GCC_EXEC_PREFIX");
  if (cp)
    {
      attempt = (char *) alloca (strlen (cp) + 50);
      /* The exec prefix can be something like
	 /usr/local/bin/../lib/gcc-lib/.  We want to change this
	 into a pointer to the share/java directory.  We support two
	 configurations: one where prefix and exec-prefix are the
	 same, and one where exec-prefix is `prefix/SOMETHING'.  */
      strcpy (attempt, cp);
      strcat (attempt, DIR_UP);
      strcat (attempt, sep);
      strcat (attempt, DIR_UP);
      strcat (attempt, sep);
      len = strlen (attempt);

      strcpy (attempt + len, "share");
      strcat (attempt, sep);
      strcat (attempt, "java");
      strcat (attempt, sep);
      strcat (attempt, "libgcj-" DEFAULT_TARGET_VERSION ".jar");
      if (! stat (attempt, &stat_b))
	{
	  add_entry (&sys_dirs, attempt, 1);
	  found = 1;
	  strcpy (&attempt[strlen (attempt)
			   - strlen ("libgcj-" DEFAULT_TARGET_VERSION ".jar")],
		  sep);
	  strcat (attempt, "ext");
	  strcat (attempt, sep);
	  if (! stat (attempt, &stat_b))
	    jcf_path_extdirs_arg (attempt);
	}
      else
	{
	  strcpy (attempt + len, DIR_UP);
	  strcat (attempt, sep);
	  strcat (attempt, "share");
	  strcat (attempt, sep);
	  strcat (attempt, "java");
	  strcat (attempt, sep);
	  strcat (attempt, "libgcj-" DEFAULT_TARGET_VERSION ".jar");
	  if (! stat (attempt, &stat_b))
	    {
	      add_entry (&sys_dirs, attempt, 1);
	      found = 1;
	      strcpy (&attempt[strlen (attempt)
			       - strlen ("libgcj-" DEFAULT_TARGET_VERSION ".jar")],
		      sep);
	      strcat (attempt, "ext");
	      strcat (attempt, sep);
	      if (! stat (attempt, &stat_b))
		jcf_path_extdirs_arg (attempt);
	    }
	}
    }
  if (! found)
    {
      /* Desperation: use the installed one.  */
      char *extdirs;
      add_entry (&sys_dirs, LIBGCJ_ZIP_FILE, 1);
      extdirs = (char *) alloca (strlen (LIBGCJ_ZIP_FILE) + 1);
      strcpy (extdirs, LIBGCJ_ZIP_FILE);
      strcpy (&extdirs[strlen (LIBGCJ_ZIP_FILE)
		      - strlen ("libgcj-" DEFAULT_TARGET_VERSION ".jar")],
	      "ext");
      strcat (extdirs, sep);
      if (! stat (extdirs, &stat_b))
	jcf_path_extdirs_arg (extdirs);
    }

  cp = getenv ("CLASSPATH");
  add_path (&classpath_env, cp, 0);
}

/* Call this when -classpath is seen on the command line.
   This overrides only the $CLASSPATH environment variable.
 */
void
jcf_path_classpath_arg (const char *path)
{
  free_entry (&classpath_user);
  add_path (&classpath_user, path, 0);
}

/* Call this when -bootclasspath is seen on the command line.
 */
void
jcf_path_bootclasspath_arg (const char *path)
{
  free_entry (&sys_dirs);
  add_path (&sys_dirs, path, 1);
}

/* Call this when -extdirs is seen on the command line.
 */
void
jcf_path_extdirs_arg (const char *cp)
{
  const char *startp, *endp;

  free_entry (&extensions);

  if (cp)
    {
      char *buf = (char *) alloca (strlen (cp) + 3);
      startp = endp = cp;
      while (1)
	{
	  if (! *endp || *endp == PATH_SEPARATOR)
	    {
	      if (endp == startp)
		return;

	      strncpy (buf, startp, endp - startp);
	      buf[endp - startp] = '\0';

	      {  
		DIR *dirp = NULL;
		int dirname_length = strlen (buf);
		
		dirp = opendir (buf);
		if (dirp == NULL)
		  return;
		
		for (;;)
		  {
		    struct dirent *direntp = readdir (dirp);
		    
		    if (!direntp)
		      break;
		    
		    if (direntp->d_name[0] != '.')
		      {
			char *name = (char *) alloca (dirname_length
					     + strlen (direntp->d_name) + 2);
			strcpy (name, buf);
			if (! IS_DIR_SEPARATOR (name[dirname_length-1]))
			  {
			    name[dirname_length] = DIR_SEPARATOR;
			    name[dirname_length+1] = 0;
			  }
			strcat (name, direntp->d_name);
			add_entry (&extensions, name, 0);
		      }
		  }
		if (dirp)
		  closedir (dirp);
	      }

	      if (! *endp)
		break;
	      ++endp;
	      startp = endp;
	    }
	  else
	    ++endp;
	}
    }
}

/* Call this when -I is seen on the command line.  */
void
jcf_path_include_arg (const char *path)
{
  add_entry (&include_dirs, path, 0);
}

/* We `seal' the path by linking everything into one big list.  Then
   we provide a way to iterate through the sealed list.  If PRINT is
   true then we print the final class path to stderr.  */
void
jcf_path_seal (int print)
{
  struct entry *secondary;

  sealed = include_dirs;
  include_dirs = NULL;

  if (classpath_user)
    {
      secondary = classpath_user;
      classpath_user = NULL;
    }
  else
    {
      if (! classpath_env)
	add_entry (&classpath_env, ".", 0);

      secondary = classpath_env;
      classpath_env = NULL;
    }


  free_entry (&classpath_user);
  free_entry (&classpath_env);

  append_entry (&sealed, secondary);
  append_entry (&sealed, sys_dirs);
  append_entry (&sealed, extensions);
  sys_dirs = NULL;
  extensions = NULL;

  if (print)
    {
      struct entry *ent;
      fprintf (stderr, "Class path starts here:\n");
      for (ent = sealed; ent; ent = ent->next)
	{
	  fprintf (stderr, "    %s", ent->name);
	  if ((ent->flags & FLAG_SYSTEM))
	    fprintf (stderr, " (system)");
	  if ((ent->flags & FLAG_ZIP))
	    fprintf (stderr, " (zip)");
	  fprintf (stderr, "\n");
	}
    }
}

void *
jcf_path_start (void)
{
  return (void *) sealed;
}

void *
jcf_path_next (void *x)
{
  struct entry *ent = (struct entry *) x;
  return (void *) ent->next;
}

static const char
PATH_SEPARATOR_STR[] = {PATH_SEPARATOR, '\0'};

char *
jcf_path_compute (const char *prefix)
{
  struct entry *iter;
  char *result;
  int length = strlen (prefix) + 1;
  int first;

  for (iter = sealed; iter != NULL; iter = iter->next)
    length += strlen (iter->name) + 1;

  result = (char *) xmalloc (length);
  strcpy (result, prefix);
  first = 1;
  for (iter = sealed; iter != NULL; iter = iter->next)
    {
      if (! first)
	strcat (result, PATH_SEPARATOR_STR);
      first = 0;
      strcat (result, iter->name);
      /* Ugly: we want to strip the '/' from zip entries when
	 computing a string classpath.  */
      if ((iter->flags & FLAG_ZIP) != 0)
	result[strlen (result) - 1] = '\0';
    }

  return result;
}

/* We guarantee that the return path will either be a zip file, or it
   will end with a directory separator.  */
char *
jcf_path_name (void *x)
{
  struct entry *ent = (struct entry *) x;
  return ent->name;
}

int
jcf_path_is_zipfile (void *x)
{
  struct entry *ent = (struct entry *) x;
  return (ent->flags & FLAG_ZIP);
}

int
jcf_path_is_system (void *x)
{
  struct entry *ent = (struct entry *) x;
  return (ent->flags & FLAG_SYSTEM);
}

int
jcf_path_max_len (void)
{
  return longest_path;
}
