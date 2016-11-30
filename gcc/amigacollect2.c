/* GG-local whole file: dynamic libraries */
/* Supplimentary functions that get compiled and linked to collect2 for
   AmigaOS target.
   Copyright (C) 1996 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"

/* From collect2.c:  */

void maybe_unlink(const char *);
void fatal_error(location_t, const char *, ...);
void fork_execute(const char *, char **, bool);

extern char *c_file_name;
extern int debug;

/* Local functions.  */

static void safename (char *);
static void add_lib (const char *);
static void cat (const char *, FILE *);

/* Names of temporary files we create.  */
#define XLIBS_C_NAME "xlibs.c"
#define XLIBS_O_NAME "xlibs.o"
#define SHARED_X_NAME "shared.x"

/* Suffix which is prepended to "-l" options for dynamic libraries.  */
#define DYNAMIC_LIB_SUFFIX "_ixlibrary"

/* Structure that holds library names.  */
struct liblist
{
  struct liblist *next;
  char *name;
  char *cname;
};

/* Not zero if "-static" was specified on GCC command line or if all the
   libraries are static.  */
static int flag_static=0;

/* Not zero if linking a base relative executable.  This is recognized by
   presence of "-m amiga_bss" on the linker's commandline.  */
static int flag_baserel=0;

/* Not zero if some of the specified libraries are dynamic.  */
static int found_dynamic_libs=0;

/* List of linker libraries.  */
struct liblist *head = NULL;

/* Return 1 if collect2 should do something more apart from tlink. We want it
   to call "postlink" and "strip" if linking with dynamic libraries.  */

int
amigaos_do_collecting (void)
{
  return !flag_static;
}

/* Check for presence of "-static" on the GCC command line.  We should not do
   collecting if this flag was specified.  */

void
amigaos_gccopts_hook (const char *arg)
{
  if (strncmp(arg, "-static", strlen("-static"))==0)
    flag_static=1;
}

/* Replace unprintable characters with underscores.  Used by "add_lib()".  */

static void
safename (char *p)
{
  if (!ISALPHA(*p))
    *p = '_';
  p++;
  while (*p)
    {
      if (!ISALNUM(*p))
	*p = '_';
      p++;
    }
}

/* Add a library to the list of dynamic libraries.  First make sure that the
   library is actually dynamic.  Used by "amigaos_libname_hook()".  */

static void
add_lib (const char *name)
{
  struct liblist *lib;
  static char buf[256];

  for (lib = head; lib; lib = lib->next)
    if (!strcmp(lib->name, name))
      return;

  /* A2IXDIR_PREFIX is passed by "make".  */
  sprintf(buf, A2IXDIR_PREFIX "/ldscripts/%s.x", name);
  if (access(buf, R_OK))
    return;

  lib = (struct liblist*)xmalloc(sizeof(struct liblist));
  lib->name = xstrdup(name);
  lib->cname = xstrdup(name);
  safename(lib->cname);
  lib->next = head;
  head = lib;

  if (debug)
    fprintf(stderr, "found dynamic library, name: %s, cname: %s\n", lib->name,
	    lib->cname);

  found_dynamic_libs=1;
}

/* Check if the argument is a linker library.  Call "add_lib()" if yes.  */

void
amigaos_libname_hook (const char *arg)
{
  int len = strlen(arg);
  if (flag_static)
    return;

  if (len > 2 && !memcmp(arg, "-l", 2))
    add_lib(arg + 2);
  else if (len > 2 && !strcmp(arg + len - 2, ".a"))
    {
      const char *lib;

      ((char*)arg)[len - 2] = '\0';
      lib = strrchr(arg, '/');
      if (lib == NULL)
	lib = strrchr(arg, ':');
      if (lib == NULL)
	lib = arg - 1;
      if (!strncmp(lib + 1, "lib", 3))
	add_lib(lib + 4);
      ((char *)arg)[len - 2] = '.';
    }
}

/* Delete temporary files.  */

void
amigaos_collect2_cleanup (void)
{
  if (flag_static)
    return;
  maybe_unlink(XLIBS_C_NAME);
  maybe_unlink(XLIBS_O_NAME);
  maybe_unlink(SHARED_X_NAME);
}

/* Copy file named by FNAME to X.  */

static void
cat (const char *fname, FILE *x)
{
#define BUFSIZE 16384
  FILE *in;
  static char buf[BUFSIZE];
  int bytes;
  
  in = fopen(fname, "r");
  if (in == NULL)
    fatal_error (input_location, "%s", fname);
  while (!feof(in) && (bytes = fread(buf, 1, BUFSIZE, in)))
    fwrite(buf, 1, bytes, x);
  fclose(in);
}

/* If no dynamic libraries were found, perform like "-static".  Otherwise,
   create "xlibs.c", "shared.x" and invoke "gcc" to create "xlibs.o".  We also
   have to adjust the linker commandline.  */

void
amigaos_prelink_hook (const char **ld1_argv, int *strip_flag)
{
  if (flag_static)
    return;

  if (!found_dynamic_libs)
    {
      flag_static=1;
      /* If the user has not requested "-static", but has requested "-s",
	 collect2 removes "-s" from the "ld1_argv", and calls "strip" after
	 linking.  However, this would not be efficient if we linked the
	 executable without any dynamic library.  In this case, we put "-s"
	 back.  */
      if (*strip_flag)
	{
	  /* Add "-s" as the last argument on the command line.  */
	  while (*ld1_argv)
	    ld1_argv++;
	  *ld1_argv++="-s";
	  *ld1_argv=0;
	  *strip_flag=0;
	}
    }
  else
    {
      FILE *x, *out;
      struct liblist *lib;
      static const char* argv[]={0, "-c", XLIBS_C_NAME, 0};
      const char **ld1_end, **ld1;

      /* Prepend suffixes to dynamic lib names. In addition, check if we are
	 linking a base relative executable.  */
      for (ld1=ld1_argv; *ld1; ld1++)
	{
	  int len=strlen(*ld1);
	  if (strncmp(*ld1, "-l", strlen("-l"))==0)
	    {
	      for (lib=head; lib; lib=lib->next)
		if (strcmp(*ld1+strlen("-l"), lib->name)==0)
		  {
		    char *newname=(char*)
			    xmalloc(strlen(*ld1)+strlen(DYNAMIC_LIB_SUFFIX)+1);
		    strcpy(newname, *ld1);
		    strcat(newname, DYNAMIC_LIB_SUFFIX);
		    *ld1=newname;
		    break;
		  }
	    }
	  else if (len > 2 && !strcmp(*ld1 + len - 2, ".a"))
	    {
	      const char *libname;
	      int substituted=0;

	      ((char *)(*ld1))[len - 2] = '\0';
	      libname = strrchr(*ld1, '/');
	      if (libname == NULL)
		libname = strrchr(*ld1, ':');
	      if (libname == NULL)
		libname = *ld1 - 1;
	      if (!strncmp(libname + 1, "lib", 3))
		for (lib=head; lib; lib=lib->next)
		  if (strcmp(libname+4, lib->name)==0)
		    {
		      char *newname=(char*)xmalloc(strlen(*ld1)+
					    strlen(DYNAMIC_LIB_SUFFIX)+3);
		      strcpy(newname, *ld1);
		      strcat(newname, DYNAMIC_LIB_SUFFIX);
		      strcat(newname, ".a");
		      *ld1=newname;
		      substituted=1;
		      break;
		    }
	      if (!substituted)
		((char *)(*ld1))[len - 2] = '.';
	    }
	  else if (strcmp(ld1[0], "-m")==0 && ld1[1]
		   && strcmp(ld1[1], "amiga_bss")==0)
	    {
	      flag_baserel=1;
	      break;
	    }
	}

      out = fopen(XLIBS_C_NAME, "w");
      if (out == NULL)
	fatal_error (input_location, "%s", XLIBS_C_NAME);
      x = fopen(SHARED_X_NAME, "w");
      if (x == NULL)
	fatal_error (input_location, "%s", SHARED_X_NAME);

      cat((flag_baserel ? A2IXDIR_PREFIX "/amiga_exe_baserel_script.x"
			: A2IXDIR_PREFIX "/amiga_exe_script.x"), x);
      for (lib = head; lib; lib = lib->next)
	{
	  static char buf[256];
	  sprintf(buf, A2IXDIR_PREFIX "/ldscripts/%s.x", lib->name);
	  fprintf(out, "extern long %sBase; long *__p%sBase = &%sBase;\n",
		  lib->cname, lib->cname, lib->cname);
	  cat(buf, x);
	} /* {{ */
      fprintf(x, "}}\n");
      fclose(out);
      fclose(x);
      argv[0]=c_file_name;
      fork_execute("gcc", (char **)argv, false);

      /* Unfortunately, unlike "-s", "-T" cannot be specified as the last
	 argument. We put it after "-L" args.  */
      ld1_end=ld1_argv;
      while (*ld1_end)
	ld1_end++;
      ld1_end++;
      /* "ld1_end" now points after the terminating 0 of "ld1_argv".  */

      ld1=ld1_end-2;
      while (ld1>ld1_argv && strncmp(*ld1, "-L", strlen("-L")))
	ld1--;
      if (ld1==ld1_argv)
	fatal_error (input_location, "no -L arguments");
      ld1++;
      /* "ld1" now points after "-L".  */

      /* Shift all the arguments after "-L" one position right.  */
      memmove(ld1+1, ld1, (ld1_end-ld1)*sizeof(*ld1));
      /* Put -Tshared.x in the now empty space.  */
      *ld1="-T" SHARED_X_NAME;
    }
}

/* Be lazy and just call "postlink".  */

void
amigaos_postlink_hook (const char *output_file)
{
  static const char *argv[]={"postlink", 0, 0, 0};
  if (flag_static)
    return;

  if (flag_baserel)
    {
      argv[1]="-baserel";
      argv[2]=output_file;
    }
  else
    argv[1]=output_file;
  fork_execute("postlink", (char **)argv, false);
}
