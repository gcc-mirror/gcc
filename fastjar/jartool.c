/*
  jartool.c - main functions for fastjar utility
  Copyright (C) 2002, 2004  Free Software Foundation
  Copyright (C) 1999, 2000, 2001  Bryan Burns
  
  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

/*
   Revision 1.10  2002/01/03 04:57:56  rodrigc
   2001-01-02  Craig Rodrigues  <rodrigc@gcc.gnu.org>

           PR bootstrap/5117
           * configure.in (AC_CHECK_HEADERS): Check for stdlib.h.
           * Makefile.am: Move grepjar to bin_PROGRAMS.
           * config.h.in: Regenerated.
           * Makefile.in: Regenerated.
           * aclocal.m4: Regenerated.
           * jargrep.c: Eliminate some signed/unsigned and default
           uninitialized warnings. Use HAVE_STDLIB_H instead of
           STDC_HEADERS macro.
           * jartool.c: Likewise.
           * compress.c: Likewise.

   Revision 1.9  2001/10/12 00:49:42  bryce
           * jatool.c (extract_jar): Account for null termination when
   	determining whether to expand "filename".

   Revision 1.8  2001/08/29 01:35:31  apbianco
   2001-08-28  Alexandre Petit-Bianco  <apbianco@redhat.com>

   	* jartool.c (add_to_jar): Return 1 if `stat' initialy failed.
   	Fixes PR java/3949.

   (http://gcc.gnu.org/ml/gcc-patches/2001-08/msg01641.html)

   Revision 1.7  2001/08/27 23:09:37  tromey
   	* jartool.c (jarfile): Remove length limitation.
   	(main): Use jt_strdup when initializing jarfile.

   Revision 1.6  2001/07/04 18:33:53  tromey
   	Modified from patch by Julian Hall <jules@acris.co.uk>:
   	* jartool.c (errno): Conditionally declare.
   	(O_BINARY): Conditionally define.
   	(main): Use open, not creat.  Use O_BINARY everywhere.
   	(make_manifest): Use O_BINARY.
   	(add_to_jar): Likewise.

   Revision 1.5  2001/05/03 21:40:47  danglin
   	* jartool.c (jt_strdup): New function.
   	(get_next_arg): Use jt_strdup instead of strdup.

   Revision 1.4  2000/12/28 21:47:37  robertl
   2000-12-28  Robert Lipe <robertl@sco.com>

           * jartool.c (MAXPATHLEN): Provide if not defined.

   Revision 1.3  2000/12/14 18:45:35  ghazi
   Warning fixes:

   	* compress.c: Include stdlib.h and compress.h.
   	(rcsid): Delete.
   	(report_str_error): Make static.
   	(ez_inflate_str): Delete unused variable.  Add parens in if-stmt.
   	(hrd_inflate_str): Likewise.

   	* compress.h (init_compression, end_compression, init_inflation,
   	end_inflation): Prototype void arguments.

   	* dostime.c (rcsid): Delete.

   	* jargrep.c: Include ctype.h, stdlib.h, zlib.h and compress.h.
   	Make functions static.  Cast ctype function argument to `unsigned
   	char'.  Add parens in if-stmts.  Constify.
   	(Usage): Change into a macro.
   	(jargrep): Remove unused parameter.

   	* jartool.c: Constify.  Add parens in if-stmts.  Align
   	signed/unsigned char pointers in functions calls using casts.
   	(rcsid): Delete.
   	(list_jar): Fix printf format specifier.
   	(usage): Chop long string into bits.  Reformat.

   	* pushback.c (rcsid): Delete.

   Revision 1.2  2000/12/13 18:11:57  tromey
   	* jartool.c (extract_jar): Use strchr, not index.

   Revision 1.1  2000/12/09 03:08:23  apbianco
   2000-12-08  Alexandre Petit-Bianco  <apbianco@cygnus.com>

           * fastjar: Imported.

   Revision 1.5  2000/08/24 15:01:27  cory
   Made certain that fastjar opened the jar file before trying to update it
   with the -u option.

   Revision 1.4  2000/08/24 13:39:21  cory
   Changed +'s to |'s in jartool.c to insure there was no confusion with sign
   when byte swapping.  Better safe than sorry.

   Revision 1.3  2000/08/23 19:42:17  cory
   Added support for more Unix platforms.  The following code has been hacked
   to work on AIX, Solaris, True 64, and HP-UX.
   Added bigendian check.  Probably works on most big and little endian platforms
   now.

   Revision 1.2  1999/12/06 07:38:28  toast
   fixed recursive archiving bug

   Revision 1.1.1.1  1999/12/06 03:09:34  toast
   initial checkin..



   Revision 1.22  1999/10/12 19:45:13  burnsbr
   adding patch to fix compat problem

   Revision 1.21  1999/05/10 09:15:49  burnsbr
   fixed manifest file version info

   Revision 1.20  1999/05/10 08:53:16  burnsbr
   *** empty log message ***

   Revision 1.19  1999/05/10 08:30:39  burnsbr
   added extract / listing code

   Revision 1.18  1999/04/28 04:24:29  burnsbr
   updated version

   Revision 1.17  1999/04/28 04:21:23  burnsbr
   added support for -C dir-changing flag.. Updated total compression display

   Revision 1.16  1999/04/27 10:28:22  burnsbr
   updated version string

   Revision 1.15  1999/04/27 10:04:06  burnsbr
   configure support

   Revision 1.14  1999/04/27 08:56:14  burnsbr
   added -V flag, better error messages

   Revision 1.13  1999/04/26 02:35:21  burnsbr
   changed all sorts of stuff.. compression now works 100%

   Revision 1.12  1999/04/23 12:00:45  burnsbr
   90% done with compression code

   Revision 1.11  1999/04/22 04:12:57  burnsbr
   finished first round of Manifest file support..
   might need to do more, digest etc..

   Revision 1.10  1999/04/22 02:35:23  burnsbr
   added more manifest support, about 75% done now.  Replaced all the
   redundant shifts and bit-logic with a macro or two, making the code
   easier to read.

   Revision 1.9  1999/04/21 09:55:16  burnsbr
   pulled out printfs

   Revision 1.8  1999/04/21 02:58:01  burnsbr
   started manifest code

   Revision 1.7  1999/04/20 23:15:28  burnsbr
   added patch sent by John Bley <jbb6@acpub.duke.edu>

   Revision 1.6  1999/04/20 08:56:02  burnsbr
   added GPL comment

   Revision 1.5  1999/04/20 08:16:09  burnsbr
   fixed verbose flag, did some optimization

   Revision 1.4  1999/04/20 05:09:59  burnsbr
   added rcsid variable

   Revision 1.3  1999/04/20 05:08:54  burnsbr
   fixed Log statement

*/

#include "config.h"

#include <zlib.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <string.h>
#include <errno.h>

#ifdef TM_IN_SYS_TIME
#include <sys/time.h>
#else
#include <time.h>
#endif

#include <getopt.h>

#include "jartool.h"
#include "zipfile.h"
#include "dostime.h"
#include "pushback.h"
#include "compress.h"
#include "shift.h"

/* Some systems have mkdir that takes a single argument.  */
#ifdef MKDIR_TAKES_ONE_ARG
# define mkdir(a,b) mkdir(a)
#endif


#ifdef WORDS_BIGENDIAN

#define L2BI(l) ((l & 0xff000000) >> 24) | \
		((l & 0x00ff0000) >> 8)  | \
		((l & 0x0000ff00) << 8)  | \
		((l & 0x000000ff) << 24);

#define L2BS(l) ((l & 0xff00) >> 8) | ((l & 0x00ff) << 8);

#endif

#ifndef errno
extern int errno;
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

void usage(const char*);
void help(const char *);
void version(void);
void add_entry(struct zipentry *);
void init_headers(void);

int consume(pb_file *, int);
int list_jar(int, char**, int);
int extract_jar(int, char**, int);
int add_file_to_jar(int, int, const char*, struct stat*, int);
int add_to_jar(int, const char*, int);
int add_to_jar_with_dir(int, const char*, const char*, int);
int create_central_header(int);
int make_manifest(int, const char*, int);
int read_entries (int);
static void init_args(char **, int);
static char *get_next_arg (void);
static char *jt_strdup (char*);
static void expand_options (int *argcp, char ***argvp);
static struct zipentry *find_entry (const char *);
static int looks_like_dir (const char *);

/* global variables */
ub1 file_header[30];
ub1 data_descriptor[16];
int do_compress;
int seekable;
int verbose;
char *jarfile;

/* If non zero, then don't recurse in directory. Instead, add the
   directory entry and relie on an explicit list of files to populate
   the archive. This option isn't supported by the original jar tool. */
int use_explicit_list_only;

/* If non zero, then read the entry names from stdin. This option
   isn't supported by the original jar tool. */
int read_names_from_stdin;

zipentry *ziplist; /* linked list of entries */
zipentry *ziptail; /* tail of the linked list */

int number_of_entries; /* number of entries in the linked list */

/* What we go by. */
const char *progname;

/* The offset of the end of the last zip entry. */
ub4 end_of_entries;

/* This is used to mark options with no short value.  */
#define LONG_OPT(Num)  ((Num) + 128)

#define OPT_HELP     LONG_OPT (0)

/* This holds all options.  */
#define OPTION_STRING "-ctxuvVf:m:C:0ME@"

/* Define the MANIFEST content here to have it easier with calculations
   below.  This is for the case we create an empty MANIFEST.MF.  */
#define MANIFEST_STR "Manifest-Version: 1.0\nCreated-By: "
#define MANIFEST_END "\n\n"

static const struct option options[] =
{
  { "help", no_argument, NULL, OPT_HELP },
  { "version", no_argument, NULL, 'V' },
  { NULL, no_argument, NULL, 0 }
};

int main(int argc, char **argv){

  char *mfile = NULL;
  
  int action = ACTION_NONE;
  int manifest = TRUE;
  int opt;
  
  int jarfd = -1;
  
  /* These are used to collect file names and `-C' options for the
     second pass through the command line.  */
  int new_argc;
  char **new_argv;

  progname = argv[0];

  do_compress = TRUE;
  verbose = FALSE;
  
  ziplist = NULL;
  
  number_of_entries = 0;
  
  if(argc < 2)
    usage(argv[0]);
  
  new_argc = 0;
  new_argv = (char **) malloc (argc * sizeof (char *));

  expand_options (&argc, &argv);
  while ((opt = getopt_long (argc, argv, OPTION_STRING,
			     options, NULL)) != -1) {
    switch(opt){
    case 'C':
      new_argv[new_argc++] = (char *) "-C";
      /* ... fall through ... */
    case 1:
      /* File name or unparsed option, due to RETURN_IN_ORDER.  */
      new_argv[new_argc++] = optarg;
      break;
    case 'c':
      action = ACTION_CREATE;
      break;
    case 't':
      action = ACTION_LIST;
      break;
    case 'x':
      action = ACTION_EXTRACT;
      break;
    case 'u':
      action = ACTION_UPDATE;
      break;
    case 'v':
      verbose = TRUE;
      break;
    case 'V':
      version();
      exit(0);
    case 'f':
      jarfile = optarg;
      break;
    case 'm':
      mfile = optarg;
      break;
    case '0':
      do_compress = FALSE;
      break;
    case 'M':
      manifest = FALSE;
      break;

    case OPT_HELP:
      help(argv[0]);
      break;

    /* The following options aren't supported by the original jar tool. */
    case 'E':
      use_explicit_list_only = TRUE;
      break;
    case '@':
      read_names_from_stdin = TRUE;
      break;
    default:
      usage(argv[0]);
    }
  }

  /* We might have seen `--'.  In this case we want to make sure that
     all following options are handled as file names.  */
  while (optind < argc)
    new_argv[new_argc++] = argv[optind++];
  new_argv[new_argc] = NULL;

  if(action == ACTION_NONE){
    fprintf(stderr, "%s: one of options -{ctxu} must be specified.\n",
	    progname);
    usage(argv[0]);
  }

  /* Verify unsupported combinations and warn of the use of non
     standard features */
  if(verbose && use_explicit_list_only)
    fprintf (stderr, "Warning: using non standard '-E' option\n");
  if(verbose && read_names_from_stdin)
    fprintf (stderr, "Warning: using non standard '-@' option\n");
  if(read_names_from_stdin
      && (action != ACTION_CREATE && action != ACTION_UPDATE)){
      fprintf(stderr, "%s: option '-@' is supported only with '-c' or '-u'.\n",
	      progname);
      usage(argv[0]);
  }

  /* create the jarfile */
  if(action == ACTION_CREATE){
    if(jarfile){
      jarfd = open(jarfile, O_CREAT | O_BINARY | O_WRONLY | O_TRUNC, 0666);

      if(jarfd < 0){
        fprintf(stderr, "%s: error opening %s for writing: %s\n", progname,
		jarfile, strerror (errno));
        exit(1);
      }
      
      /* We assume that the file is seekable */
      seekable = TRUE;
      
    } else {
      
      jarfd = STDOUT_FILENO;  /* jarfd is stdout otherwise */
      
      /* standard out is not seekable */
      seekable = FALSE;
      
      /* don't want our output to be part of the jar file.. figured this one
         out the hard way.. =P */
      verbose = FALSE;
    }
  } else if(action == ACTION_LIST || action == ACTION_EXTRACT){

    if(jarfile){
      jarfd = open(jarfile, O_RDONLY | O_BINARY);

      if(jarfd < 0){
        fprintf(stderr, "%s: error opening %s for reading: %s\n", progname,
		jarfile, strerror (errno));
        exit(1);
      }

      seekable = TRUE;
    } else {
      jarfd = STDIN_FILENO; /* jarfd is standard in */

      /* we assume that the stream isn't seekable for safety */
      seekable = FALSE;
    }
  }

  if (action == ACTION_UPDATE)
    {
      if (!jarfile)
	{
	  fprintf (stderr, "%s: `-u' mode requires a file name\n",
		   argv[0]);
	  exit (1);
	}

      if ((jarfd = open (jarfile, O_RDWR | O_BINARY)) < 0)
	{
	  fprintf (stderr, "Error opening %s for reading!\n", jarfile);
	  perror (jarfile);
	  exit (1);
	}

      /* Assert that jarfd is seekable. */
      if (lseek (jarfd, 0, SEEK_CUR) == -1)
	{
	  fprintf (stderr, "%s: %s is not seekable\n", argv[0], jarfile);
	  exit (1);
	}

      seekable = TRUE;
    }

  if(action == ACTION_CREATE || action == ACTION_UPDATE){
    const char *arg;
    init_headers();

    if(do_compress)
      init_compression();

    if (action == ACTION_UPDATE)
      {
	if (read_entries (jarfd))
	  exit (1);
      }

    /* Add the META-INF/ directory and the manifest */
    if(manifest && mfile)
      make_manifest(jarfd, mfile, action == ACTION_UPDATE);
    else if(manifest && action == ACTION_CREATE)
      make_manifest(jarfd, NULL, FALSE);

    init_args (new_argv, 0);
    /* now we add the files to the archive */
    while ((arg = get_next_arg ())){

      if(!strcmp(arg, "-C")){
	const char *dir_to_change = get_next_arg ();
	const char *file_to_add = get_next_arg ();
        if (!dir_to_change || !file_to_add) {
          fprintf(stderr, "%s: error: missing argument for -C.\n", progname);
          exit(1);
        }
	if (add_to_jar_with_dir(jarfd, dir_to_change, file_to_add,
				action == ACTION_UPDATE))
	  {
	    fprintf(stderr,
		    "Error adding %s (in directory %s) to jar archive!\n",
		    file_to_add, dir_to_change);
	    exit(1);
	  }
      } else {
        if(add_to_jar(jarfd, arg, action == ACTION_UPDATE)){
          fprintf(stderr, "Error adding %s to jar archive!\n", arg);
          exit(1);
        }
      }
    }
    /* de-initialize the compression DS */
    if(do_compress)
      end_compression();

    if (action == ACTION_UPDATE)
      lseek (jarfd, end_of_entries, SEEK_SET);
    
    create_central_header(jarfd);

#if ! (HAVE_FTRUNCATE || HAVE__CHSIZE)
  #error neither ftruncate() or _chsize() available
#endif
    /* Check if the file shrunk when we updated it. */
    if (action == ACTION_UPDATE)
#if HAVE_FTRUNCATE
      ftruncate (jarfd, lseek (jarfd, 0, SEEK_CUR));
#else
      _chsize (jarfd, lseek (jarfd, 0, SEEK_CUR));
#endif

    if (jarfd != STDIN_FILENO && close(jarfd) != 0) {
      fprintf(stderr, "%s: error closing jar archive: %s\n",
	      progname, strerror (errno));
      exit (1);
    }
  } else if(action == ACTION_LIST){
    list_jar(jarfd, &new_argv[0], new_argc);
  } else if(action == ACTION_EXTRACT){
    extract_jar(jarfd, &new_argv[0], new_argc);
  }
  
  exit(0);
}

static int args_current_g;
static char **args_g;

static void 
init_args(args, current)
     char **args;
     int current;
{
  if(!read_names_from_stdin)
    {
      args_g = args;
      args_current_g = current;
    }
}

static char *
get_next_arg ()
{
  static int reached_end = 0;

  if (reached_end)
    return NULL;

  if (args_g)
    {
      if (!args_g [args_current_g])
	{
	  reached_end = 1;
	  return NULL;
	}
      return args_g [args_current_g++];
    }
  else
    {
      /* Read the name from stdin. Delimiters are '\n' and
	 '\r'. Reading EOF indicates that we don't have anymore file
	 names characters to read. */

      char s [MAXPATHLEN];
      int  pos = 0;

      /* Get rid of '\n' and '\r' first. */
      while (1)
	{
	  int c = getc (stdin);
	  if (c == '\n' || c == '\r')
	    continue;
	  else
	    {
	      if (c == EOF)
		return NULL;
	      ungetc (c, stdin);
	      break;
	    }
	}

      while (1)
	{
	  int c = getc (stdin);
	  /* Exit when we get a delimiter or don't have any characters
             to read */
	  if (c == '\n'|| c == '\r'|| c == EOF)
	    break;
	  s [pos++] = (char) c;
	}

      if (pos)
	{
	  s [pos] = '\0';
	  return jt_strdup (s);
	}
      else
	return NULL;
    }
}

void init_headers(){
  /* packing file header */
  /* magic number */
  file_header[0] = 0x50;
  file_header[1] = 0x4b;
  file_header[2] = 0x03;
  file_header[3] = 0x04;
  /* version number (Unix 1.0)*/
  file_header[4] = 10;
  file_header[5] = 0;
  /* bit flag (normal deflation)*/
  file_header[6] = 0x00;

  file_header[7] = 0x00;
  /* do_compression method (deflation) */
  file_header[8] = 0;
  file_header[9] = 0;

  /* last mod file time (MS-DOS format) */
  file_header[10] = 0;
  file_header[11] = 0;
  /* last mod file date (MS-DOS format) */
  file_header[12] = 0;
  file_header[13] = 0;
  /* CRC 32 */
  file_header[14] = 0;
  file_header[15] = 0;
  file_header[16] = 0;
  file_header[17] = 0;
  /* compressed size */
  file_header[18] = 0;
  file_header[19] = 0;
  file_header[20] = 0;
  file_header[21] = 0;
  /* uncompressed size */
  file_header[22] = 0;
  file_header[23] = 0;
  file_header[24] = 0;
  file_header[25] = 0;
  /* filename length */
  file_header[26] = 0;
  file_header[27] = 0;
  /* extra field length */
  file_header[28] = 0;
  file_header[29] = 0;

  /* Initialize the compression DS */
  PACK_UB4(data_descriptor, 0, 0x08074b50);
  
}

void add_entry(struct zipentry *ze){

  if(ziplist == NULL){
    ziplist = ze;
    ziptail = ziplist;
  } else {
    ziplist->next_entry = ze;
    ziplist = ze;
  }
  
  number_of_entries++;
}

static struct zipentry *
find_entry (const char *fname)
{
  struct zipentry *ze;

  for (ze = ziptail; ze; ze = ze->next_entry)
    {
      if (!strcmp (ze->filename, fname))
	return ze;
    }
  return NULL;
}


static int
looks_like_dir (const char *fname)
{
  struct zipentry *ze;
  size_t len = strlen (fname);

  for (ze = ziptail; ze; ze = ze->next_entry)
    {
      if (strlen (ze->filename) > len
	  && !strncmp (fname, ze->filename, len)
	  && ze->filename[len] == '/')
	return 1;
    }
  return 0;
}


/*
 * Read the zip entries of an existing file, building `ziplist' as we go.
 */
int read_entries (int fd)
{
  struct zipentry *ze;
  ub1 intbuf[4];
  ub1 header[46];
  ub2 len;
  ub2 count, i;
  off_t offset;

  if (lseek (fd, -22, SEEK_END) == -1)
    {
      fprintf (stderr, "%s: %s: can't seek file\n", progname, jarfile);
      return 1;
    }

  if (read (fd, intbuf, 4) < 4)
    {
      perror (progname);
      return 1;
    }
  /* Is there a zipfile comment? */
  while (UNPACK_UB4(intbuf, 0) != 0x06054b50)
    {
      if (lseek (fd, -5, SEEK_CUR) == -1 ||
	  read (fd, intbuf, 4) != 4)
	{
	  fprintf (stderr, "%s: can't find end of central directory: %s\n",
		   progname, strerror (errno));
	  return 1;
	}
    }

  /* Skip disk numbers. */
  if (lseek (fd, 6, SEEK_CUR) == -1)
    {
      perror (progname);
      return 1;
    }

  /* Number of entries in the central directory. */
  if (read (fd, intbuf, 2) != 2)
    {
      perror (progname);
      return 1;
    }
  count = UNPACK_UB2(intbuf, 0);

  if (lseek (fd, 4, SEEK_CUR) == -1)
    {
      perror (progname);
      return 1;
    }

  /* Offset where the central directory begins. */
  if (read (fd, intbuf, 4) != 4)
    {
      perror (progname);
      return 1;
    }
  offset = UNPACK_UB4(intbuf, 0);
  end_of_entries = offset;

  if (lseek (fd, offset, SEEK_SET) != offset)
    {
      perror (progname);
      return 1;
    }

  if (read (fd, header, 46) != 46)
    {
      fprintf (stderr, "%s: %s: unexpected end of file\n",
	       progname, jarfile);
      return 1;
    }

  for (i = 0; i < count; i++)
    {
      if (UNPACK_UB4(header, 0) != 0x02014b50)
	{
	  fprintf (stderr, "%s: can't find central directory header\n",
		   progname);
	  return 1;
	}
      ze = (struct zipentry *) malloc (sizeof (struct zipentry));
      if (!ze)
	{
	  perror (progname);
	  return 1;
	}
      memset (ze, 0, sizeof (struct zipentry));
      ze->flags = UNPACK_UB2(header, CEN_FLAGS);
      ze->mod_time = UNPACK_UB2(header, CEN_MODTIME);
      ze->mod_date = UNPACK_UB2(header, CEN_MODDATE);
      ze->crc = UNPACK_UB4(header, CEN_CRC);
      ze->usize = UNPACK_UB4(header, CEN_USIZE);
      ze->csize = UNPACK_UB4(header, CEN_CSIZE);
      ze->offset = UNPACK_UB4(header, CEN_OFFSET);
      ze->compressed = (header[CEN_COMP] || header[CEN_COMP+1]);
      len = UNPACK_UB2(header, CEN_FNLEN);
      ze->filename = (char *) malloc ((len+1) * sizeof (char));
      if (!ze->filename)
	{
	  perror (progname);
	  return 1;
	}
      if (read (fd, ze->filename, len) != len)
	{
	  fprintf (stderr, "%s: %s: unexpected end of file\n",
		   progname, jarfile);
	  return 1;
	}
      len = UNPACK_UB4(header, CEN_EFLEN);
      len += UNPACK_UB4(header, CEN_COMLEN);
      if (lseek (fd, len, SEEK_CUR) == -1)
	{
	  perror (progname);
	  return 1;
	}
      add_entry (ze);
      if (i < count - 1)
	{
	  if (read (fd, header, 46) != 46)
	    {
	      fprintf (stderr, "%s: %s: unexpected end of file\n",
		       progname, jarfile);
	      return 1;
	    }
	}
    }

  lseek (fd, 0, SEEK_SET);
  return 0;
}

int make_manifest(int jfd, const char *mf_name, int updating){
  time_t current_time;
  int nlen;   /* length of file name */
  int mod_time; /* file modification time */
  struct zipentry *ze;
  
  nlen = 9;  /* trust me on this one */

  memset((file_header + 12), '\0', 16); /*clear mod time, crc, size fields*/
  
  current_time = time(NULL);
  if(current_time == (time_t)-1){
    perror("time");
    exit(1);
  }

  mod_time = unix2dostime(&current_time);
  
  PACK_UB2(file_header, LOC_EXTRA, 0);
  PACK_UB2(file_header, LOC_COMP, 0);
  PACK_UB2(file_header, LOC_FNLEN, nlen);
  PACK_UB4(file_header, LOC_MODTIME, mod_time);
  
  if(verbose)
    printf("adding: META-INF/ (in=0) (out=0) (stored 0%%)\n");
  
  ze = (zipentry*)malloc(sizeof(zipentry));
  if(ze == NULL){
    perror("malloc");
    exit(1);
  }
  
  memset(ze, 0, sizeof(zipentry)); /* clear all the fields*/
  ze->filename = (char*)malloc((nlen + 1) * sizeof(char) + 1);
  strcpy(ze->filename, "META-INF/");
  ze->filename[nlen] = '\0';
    
  ze->offset = lseek(jfd, 0, SEEK_CUR);
  ze->mod_time = (ub2)(mod_time & 0x0000ffff);
  ze->mod_date = (ub2)((mod_time & 0xffff0000) >> 16);
  ze->compressed = FALSE;

  add_entry(ze);
  
  write(jfd, file_header, 30);
  write(jfd, "META-INF/", nlen);

  /* if the user didn't specify an external manifest file... */
  if(mf_name == NULL){
    
    int mf_len = strlen(MANIFEST_STR) + strlen(VERSION) + strlen(MANIFEST_END);
    char *mf;

    if((mf = (char *) malloc(mf_len + 1))) {
    uLong crc;

    sprintf(mf, "%s%s%s", MANIFEST_STR, VERSION, MANIFEST_END);

    crc = crc32(0L, Z_NULL, 0);
    
    crc = crc32(crc, (const unsigned char *)mf, mf_len);

    nlen = 20;  /* once again, trust me */

    PACK_UB2(file_header, LOC_EXTRA, 0);
    PACK_UB2(file_header, LOC_COMP, 0);
    PACK_UB2(file_header, LOC_FNLEN, nlen);
    PACK_UB4(file_header, LOC_USIZE, mf_len);
    
    memcpy((file_header + LOC_CSIZE), (file_header + LOC_USIZE), 4);
    
    PACK_UB4(file_header, LOC_CRC, crc);

    if(verbose)
      printf("adding: META-INF/MANIFEST.MF (in=56) (out=56) (stored 0%%)\n");
    
    ze = (zipentry*)malloc(sizeof(zipentry));
    if(ze == NULL){
      perror("malloc");
      exit(1);
    }
    
    memset(ze, 0, sizeof(zipentry)); /* clear all the fields*/
    ze->filename = (char*)malloc((nlen + 1) * sizeof(char) + 1);
    strcpy(ze->filename, "META-INF/MANIFEST.MF");
    ze->filename[nlen] = '\0';
    
    ze->offset = lseek(jfd, 0, SEEK_CUR);
    ze->mod_time = (ub2)(mod_time & 0x0000ffff);
    ze->mod_date = (ub2)((mod_time & 0xffff0000) >> 16);
    ze->crc = crc;
    ze->csize = mf_len;
    ze->usize = ze->csize;
    ze->compressed = FALSE;
    
    add_entry(ze);
    
    write(jfd, file_header, 30);
    write(jfd, "META-INF/MANIFEST.MF", nlen);
    write(jfd, mf, mf_len);
    free(mf);
    }
    else {
	printf("malloc errror\n");
	exit(-1);
    }
  } else {
    int mfd;
    struct stat statbuf;

    stat(mf_name, &statbuf);

    if(!S_ISREG(statbuf.st_mode)){
      fprintf(stderr, "Invalid manifest file specified.\n");
      exit(1);
    }
  
    mfd = open(mf_name, O_RDONLY | O_BINARY);

    if(mfd < 0){
      fprintf(stderr, "Error opening %s.\n", mf_name);
      exit(1);
    }

    if(add_file_to_jar(jfd, mfd, "META-INF/MANIFEST.MF", &statbuf, updating)){
      perror("error writing to jar");
      exit(1);
    }

  }

  return 0;
}

/* Implements -C by wrapping add_to_jar.  new_dir is the directory 
   to switch to.

   `updating', if nonzero, will indicate that we are updating an
   existing file, and will need to take special care. If set, we will
   also expect that the linked list of zip entries will be filled in
   with the jar file's current contents.
 */
int 
add_to_jar_with_dir (int fd, const char* new_dir, const char* file,
		     const int updating)
{
  int retval;
  char old_dir[MAXPATHLEN]; 
  if (getcwd(old_dir, MAXPATHLEN) == NULL) {
    perror("getcwd");
    return 1;
  }
  if (chdir(new_dir) == -1) {
    perror(new_dir);
    return 1;
  }
  retval=add_to_jar(fd, file, updating);
  if (chdir(old_dir) == -1) {
    perror(old_dir);
    return 1;
  }
  return retval;
}

int 
add_to_jar (int fd, const char *file, const int updating)
{
  struct stat statbuf;
  DIR *dir;
  struct dirent *de;
  zipentry *ze;
  zipentry *existing = NULL;
  int stat_return;

  /* This is a quick compatibility fix -- Simon Weijgers <simon@weijgers.com> 
   * It fixes this:
   *   "normal" jar : org/apache/java/io/LogRecord.class
   *   fastjar      : ./org/apache/java/io/LogRecord.class
   * Fastjar's preservation of the ./'s makes the jarfile unusuable for use 
   * with both kaffe-1.0b4 and JDK.
   */
  while (*file=='.' && *(file+1)=='/')
    file+=2;
  
  if(jarfile && !strcmp(file, jarfile)){
    if(verbose)
      printf("skipping: %s\n", file);
    return 0;  /* we don't want to add ourselves.. */
  }

  stat_return = stat(file, &statbuf);
  
  if(stat_return == -1){
    perror(file);
    return 1;
  } else if(S_ISDIR(statbuf.st_mode)){
    char *fullname;
    char *t_ptr;
    int nlen;
    unsigned long mod_time;

    dir = opendir(file);
    
    if(dir == NULL){
      perror("opendir");
      return 1;
    }
    
    nlen = strlen(file) + 256;
    fullname = (char*)malloc(nlen * sizeof(char));
    memset(fullname, 0, (nlen * sizeof(char)));
    
    if(fullname == NULL){
      fprintf(stderr, "Filename is NULL!\n");
      return 1;
    }

    strcpy(fullname, file);
    nlen = strlen(file);

    if(fullname[nlen - 1] != '/'){
      fullname[nlen] = '/';
      t_ptr = (fullname + nlen + 1);
    } else
      t_ptr = (fullname + nlen);


    memset((file_header + 12), '\0', 16); /*clear mod time, crc, size fields*/
    
    nlen = (t_ptr - fullname);

    mod_time = unix2dostime(&statbuf.st_mtime);

    PACK_UB2(file_header, LOC_EXTRA, 0);
    PACK_UB2(file_header, LOC_COMP, 0);
    PACK_UB2(file_header, LOC_FNLEN, nlen);
    PACK_UB4(file_header, LOC_MODTIME, mod_time);

    ze = (zipentry*)malloc(sizeof(zipentry));
    if(ze == NULL){
      perror("malloc");
      exit(1);
    }

    memset(ze, 0, sizeof(zipentry)); /* clear all the fields*/
    ze->filename = (char*)malloc((nlen + 1) * sizeof(char) + 1);
    strcpy(ze->filename, fullname);
    ze->filename[nlen] = '\0';
    
    ze->offset = lseek(fd, 0, SEEK_CUR);
    ze->mod_time = (ub2)(mod_time & 0x0000ffff);
    ze->mod_date = (ub2)((mod_time & 0xffff0000) >> 16);
    ze->compressed = FALSE;

    if (updating)
      {
	if ((existing = find_entry (ze->filename)) != NULL)
	  {
	    if (existing->usize != 0)
	      {
		/* XXX overwriting non-directory with directory? */
		fprintf (stderr, "%s: %s: can't overwrite non-directory with directory\n",
			 progname, fullname);
		return 1;
	      }
	  }
	if (lseek (fd, end_of_entries, SEEK_SET) == -1)
	  {
	    fprintf (stderr, "%s %d\n", __FILE__, __LINE__);
	    perror ("lseek");
	    return 1;
	  }
      }

    if (!existing)
      {
	add_entry (ze);
	write (fd, file_header, 30);
	write (fd, fullname, nlen);
	end_of_entries = lseek (fd, 0, SEEK_CUR);

	if (verbose)
	  printf ("adding: %s (in=%d) (out=%d) (stored 0%%)\n", fullname, 0, 0);
      }

    while(!use_explicit_list_only && (de = readdir(dir)) != NULL){
      if(de->d_name[0] == '.')
        continue;
      if(jarfile && !strcmp(de->d_name, jarfile)){
	/* we don't want to add ourselves.  Believe me */
        if(verbose)
          printf("skipping: %s\n", de->d_name);
        continue;
      }

      strcpy(t_ptr, de->d_name);

      if (add_to_jar(fd, fullname, updating)) {
        fprintf(stderr, "Error adding file to jar!\n");
        return 1;
      }
    }

    free(fullname);
    closedir(dir);
      
  } else if(S_ISREG(statbuf.st_mode)){
    int add_fd;

    add_fd = open(file, O_RDONLY | O_BINARY);
    if(add_fd < 0){
      fprintf(stderr, "Error opening %s.\n", file);
      return 1;
    }
    
    if(add_file_to_jar(fd, add_fd, file, &statbuf, updating)){
      fprintf(stderr, "Error adding file to jar!\n");
      return 1;
    }
    
  } else {
    fprintf(stderr, "Illegal file specified: %s\n", file);
  }
  return 0;
}

int add_file_to_jar(int jfd, int ffd, const char *fname, struct stat *statbuf,
		    const int updating)
{
  unsigned short file_name_length;
  unsigned long mod_time;
  ub1 rd_buff[RDSZ];
  uLong crc = 0;
  off_t offset = 0;
  int rdamt;
  struct zipentry *ze;
  struct zipentry *existing = NULL;

  if (updating)
    {
      existing = find_entry (fname);
      if (existing && looks_like_dir (fname))
	{
	  fprintf (stderr, "%s: %s is a directory in the archive\n",
		   progname, fname);
	  return 1;
	}
    }

  mod_time = unix2dostime(&(statbuf->st_mtime));
  file_name_length = strlen(fname);

  if(!seekable && !do_compress){
    crc = crc32(0L, Z_NULL, 0); 
    
    while((rdamt = read(ffd, rd_buff, RDSZ)) != 0) 
      crc = crc32(crc, rd_buff, rdamt); 
    
    lseek(ffd, 0, SEEK_SET);
  }
  
  /* data descriptor */
  if(!seekable && do_compress){
    PACK_UB2(file_header, LOC_EXTRA, 8);
  } else {
    PACK_UB2(file_header, LOC_EXTRA, 0);
  }
  
  if(do_compress){
    PACK_UB2(file_header, LOC_COMP, 8);
  } else {
    PACK_UB2(file_header, LOC_COMP, 0);
  }
    
  PACK_UB4(file_header, LOC_MODTIME, mod_time);
  PACK_UB2(file_header, LOC_FNLEN, file_name_length);
  
  if(!seekable && !do_compress){
    PACK_UB4(file_header, LOC_CRC, crc);
    PACK_UB4(file_header, LOC_USIZE, statbuf->st_size); 
    PACK_UB4(file_header, LOC_CSIZE, statbuf->st_size);
  } else 
    memset((file_header + LOC_CRC), '\0', 12); /* clear crc/usize/csize */
  
  ze = (zipentry*)malloc(sizeof(zipentry));
  if(ze == NULL){
    perror("malloc");
    exit(1);
  }
  
  memset(ze, 0, sizeof(zipentry)); /* clear all the fields*/
  ze->filename = (char*)malloc((file_name_length + 1) * sizeof(char));
  strcpy(ze->filename, fname);

  ze->mod_time = (ub2)(mod_time & 0x0000ffff);
  ze->mod_date = (ub2)((mod_time & 0xffff0000) >> 16);

  if(!seekable && !do_compress)
    ze->crc = crc;

  ze->csize = statbuf->st_size;
  ze->usize = ze->csize;

  if (existing)
    ze->offset = existing->offset;
  else if (updating)
    ze->offset = end_of_entries;
  else
    ze->offset = lseek(jfd, 0, SEEK_CUR);

  if(do_compress)
    ze->compressed = TRUE;
  else
    ze->compressed = FALSE;

  if (!existing)
    add_entry(ze);
  if (updating && lseek (jfd, ze->offset, SEEK_SET) < 0)
    {
      perror ("lseek");
      return 1;
    }

  /* We can safely write the header here, since it will be the same size
     as before */
  
  /* Write the local header */
  write(jfd, file_header, 30);
    
  /* write the file name to the zip file */
  write(jfd, fname, file_name_length);


  if(verbose){
    if (existing)
      printf ("updating: %s ", fname);
    else
      printf("adding: %s ", fname);
    fflush(stdout);
  }
 
  if(do_compress){
    /* compress the file */
    compress_file(ffd, jfd, ze, existing);
  } else {
    /* If we are not writing the last entry, make space for it. */
    if (existing && existing->next_entry)
      {
	if (ze->usize > existing->usize)
	  {
	    if (shift_down (jfd, existing->next_entry->offset,
			    ze->usize - existing->usize, existing->next_entry))
	      {
		fprintf (stderr, "%s: %s\n", progname, strerror (errno));
		return 1;
	      }
	  }
      }

    /* Write the contents of the file (uncompressed) to the zip file */
    /* calculate the CRC as we go along */
    ze->crc = crc32(0L, Z_NULL, 0); 
      
    while((rdamt = read(ffd, rd_buff, RDSZ)) != 0){
      ze->crc = crc32(ze->crc, rd_buff, rdamt);
      if(write(jfd, rd_buff, rdamt) != rdamt){
        perror("write");
        return 0;
      }
    }
  }
  close(ffd);
  
  /* write out data descriptor */
  PACK_UB4(data_descriptor, 4, ze->crc);
  PACK_UB4(data_descriptor, 8, ze->csize);
  PACK_UB4(data_descriptor, 12, ze->usize);

  /* we need to seek back and fill the header */
  if(seekable){
    offset = (ze->csize + strlen(ze->filename) + 16);
    
    if(lseek(jfd, -offset, SEEK_CUR) == (off_t)-1){
      perror("lseek");
      exit(1);
    }

    if(write(jfd, (data_descriptor + 4), 12) != 12){
      perror("write");
      return 0;
    }
    
    offset -= 12;

    if(lseek(jfd, offset, SEEK_CUR) == (off_t)-1){
      perror("lseek");
      exit(1);
    }
  } else if(do_compress){
    /* Sun's jar tool will only allow a data descriptor if the entry is
       compressed, but we'll save 16 bytes/entry if we only use it when
       we can't seek back on the file */
    /* Technically, you CAN'T have a data descriptor unless the data
       part has an obvious end, which DEFLATED does. Otherwise, there
       would not be any way to determine where the data descriptor is.
       Store an uncompressed file that ends with 0x504b0708, and see.
       -- csm */
    
    if(write(jfd, data_descriptor, 16) != 16){
      perror("write");
      return 0;
    }
  }

  if (existing)
    {
      int dd = (existing->flags & (1 << 3)) ? 12 : 0;
      if (existing->next_entry && ze->csize < existing->csize + dd)
	{
	  if (shift_up (jfd, existing->next_entry->offset,
			existing->csize + dd - ze->csize,
			existing->next_entry))
	    {
	      perror (progname);
	      return 1;
	    }
	}
      /* Replace the existing entry data with this entry's. */
      existing->csize = ze->csize;
      existing->usize = ze->usize;
      existing->crc = ze->crc;
      existing->mod_time = ze->mod_time;
      existing->mod_date = ze->mod_date;
      free (ze->filename);
      free (ze);
    }
  else if (updating)
    end_of_entries = lseek (jfd, 0, SEEK_CUR);
  
  if(verbose)
    printf("(in=%d) (out=%d) (%s %d%%)\n", 
           (int)ze->usize, (int)ze->csize,
           (do_compress ? "deflated" : "stored"),
           (do_compress ? ((int)((1 - ze->csize/(float)ze->usize) * 100)) : 0));

  return 0;
}

int create_central_header(int fd){
  ub1 header[46];
  ub1 end_header[22];
  int start_offset;
  int dir_size;
  int total_in = 0, total_out = 22;

  zipentry *ze;

  /* magic number */
  header[0] = 'P';
  header[1] = 'K';
  header[2] = 1;
  header[3] = 2;
  /* version made by */
  header[4] = 10;
  header[5] = 0;
  /* version needed to extract */
  header[6] = 10;
  header[7] = 0;
  /* bit flag */
  header[8] = 0;
  header[9] = 0;
  /* compression method */
  header[10] = 0;
  header[11] = 0;
  /* file mod time */
  header[12] = 0;
  header[13] = 0;
  /* file mod date */
  header[14] = 0;
  header[15] = 0;
  /* crc 32 */
  header[16] = 0;
  header[17] = 0;
  header[18] = 0;
  header[19] = 0;
  /* compressed size */
  header[20] = 0;
  header[21] = 0;
  header[22] = 0;
  header[23] = 0;
  /* uncompressed size */
  header[24] = 0;
  header[25] = 0;
  header[26] = 0;
  header[27] = 0;
  /* filename length */
  header[28] = 0;
  header[29] = 0;
  /* extra field length */
  header[30] = 0;
  header[31] = 0;
  /* file comment length */
  header[32] = 0;
  header[33] = 0;
  /* disk number start */
  header[34] = 0;
  header[35] = 0;
  /* internal file attribs */
  header[36] = 0;
  header[37] = 0;
  /* external file attribs */
  header[38] = 0;
  header[39] = 0;
  header[40] = 0;
  header[41] = 0;
  /* relative offset of local header */
  header[42] = 0;
  header[43] = 0;
  header[44] = 0;
  header[45] = 0;

  start_offset = lseek(fd, 0, SEEK_CUR);

  for(ze = ziptail; ze != NULL; ze = ze->next_entry){

    total_in += ze->usize;
    total_out += ze->csize + 76 + strlen(ze->filename) * 2;

    if(ze->compressed){
      PACK_UB2(header, CEN_COMP, 8);
    } else {
      PACK_UB2(header, CEN_COMP, 0);
    }
        
    PACK_UB2(header, CEN_MODTIME, ze->mod_time);
    PACK_UB2(header, CEN_MODDATE, ze->mod_date);
    PACK_UB4(header, CEN_CRC, ze->crc);
    PACK_UB4(header, CEN_CSIZE, ze->csize);
    PACK_UB4(header, CEN_USIZE, ze->usize);
    PACK_UB2(header, CEN_FNLEN, strlen(ze->filename));
    PACK_UB4(header, CEN_OFFSET, ze->offset);

    write(fd, header, 46);

    write(fd, ze->filename, strlen(ze->filename));
  }

  dir_size = lseek(fd, 0, SEEK_CUR) - start_offset;

  /* magic number */
  end_header[0] = 0x50;
  end_header[1] = 0x4b;
  end_header[2] = 0x05;
  end_header[3] = 0x06;
  /* number of this disk */
  end_header[4] = 0;
  end_header[5] = 0;
  /* number of disk w/ start of central header */
  end_header[6] = 0;
  end_header[7] = 0;
  /* total number of entries in central dir on this disk*/
  PACK_UB2(end_header, 8, number_of_entries);
  /* total number of entries in central dir*/
  PACK_UB2(end_header, 10, number_of_entries);
  /* size of central dir. */
  PACK_UB4(end_header, 12, dir_size);
  /* offset of start of central dir */
  PACK_UB4(end_header, 16, start_offset);
  /* zipfile comment length */
  end_header[20] = 0;
  end_header[21] = 0;

  write(fd, end_header, 22);
  
  if(verbose)
    printf("Total:\n------\n(in = %d) (out = %d) (%s %d%%)\n", 
           total_in, 
           total_out,
           (do_compress ? "deflated" : "stored"),
           (int)((1 - (total_out / (float)total_in)) * 100)
           );

  return 0;
}

int extract_jar(int fd, char **files, int file_num){
  int rdamt;
  int out_a, in_a;
  ub4 signature;
  ub4 csize;
  ub4 crc;
  ub2 fnlen;
  ub2 eflen;
  ub2 flags;
  ub2 method;
  ub1 *filename = NULL;
  int filename_len = 0;
  ub4 rd_buff[RDSZ];
  pb_file pbf;
  ub1 scratch[16];
  zipentry ze;
  int f_fd;
  int dir;
  int handle;
  int j;

  init_inflation();

  pb_init(&pbf, fd);

  for(;;){
    f_fd = 0;
    crc = 0;
    ze.crc = 0;
    
    dir = FALSE; /* by default, the file isn't a dir */
    handle = TRUE; /* by default we'll extract/create the file */

    if((rdamt = pb_read(&pbf, scratch, 4)) != 4){
      perror("read");
      break;
    }
    
    signature = UNPACK_UB4(scratch, 0);

#ifdef DEBUG    
    printf("signature is %x\n", signature);
#endif
    if(signature == 0x08074b50){
#ifdef DEBUG    
      printf("skipping data descriptor\n");
#endif
      pb_read(&pbf, scratch, 12);
      continue;
    } else if(signature == 0x02014b50){
#ifdef DEBUG    
      printf("Central header reached.. we're all done!\n");
#endif
      break;
    }else if(signature != 0x04034b50){
      printf("Ick! %#x\n", signature);
      break;
    }
    
    if((rdamt = pb_read(&pbf, (file_header + 4), 26)) != 26){
      perror("read");
      break;
    }
    
    csize = UNPACK_UB4(file_header, LOC_CSIZE);
#ifdef DEBUG    
    printf("Compressed size is %u\n", csize);
#endif

    fnlen = UNPACK_UB2(file_header, LOC_FNLEN);
#ifdef DEBUG    
    printf("Filename length is %hu\n", fnlen);
#endif

    eflen = UNPACK_UB2(file_header, LOC_EFLEN);
#ifdef DEBUG    
    printf("Extra field length is %hu\n", eflen);
#endif

    flags = UNPACK_UB2(file_header, LOC_EXTRA);
#ifdef DEBUG    
    printf("Flags are %#hx\n", flags);
#endif

    method = UNPACK_UB2(file_header, LOC_COMP);
#ifdef DEBUG
    printf("Compression method is %#hx\n", method);
#endif

    /* if there isn't a data descriptor */
    if(!(flags & 0x0008)){
      crc = UNPACK_UB4(file_header, LOC_CRC);
#ifdef DEBUG    
      printf("CRC is %x\n", crc);
#endif
    }

    if(filename_len < fnlen + 1){
      if(filename != NULL)
        free(filename);
      
      filename = malloc(sizeof(ub1) * (fnlen + 1));
      filename_len = fnlen + 1;
    }

    pb_read(&pbf, filename, fnlen);
    filename[fnlen] = '\0';

#ifdef DEBUG    
    printf("filename is %s\n", filename);
#endif

    if(file_num > 0){
      handle = FALSE;
      
      for(j = 0; j < file_num; j++)
        if(strcmp(files[j], (const char *)filename) == 0){
          handle = TRUE;
          break;
        }
    }

    if(!handle)
      f_fd = -1;

    /* OK, there is some directory information in the file.  Nothing to do
       but ensure the directory(s) exist, and create them if they don't.
       What a pain! */
    if(strchr((const char *)filename, '/') != NULL && handle){
      /* Loop through all the directories in the path, (everything w/ a '/') */
      const ub1 *start = filename;
      char *tmp_buff;
      struct stat sbuf;

      tmp_buff = malloc(sizeof(char) * strlen((const char *)filename));

      for(;;){
        const ub1 *idx = (const unsigned char *)strchr((const char *)start, '/');

        if(idx == NULL)
          break;
        else if(idx == start){
          start++;
          continue;
        }
        start = idx + 1;

        strncpy(tmp_buff, (const char *)filename, (idx - filename));
        tmp_buff[(idx - filename)] = '\0';

#ifdef DEBUG    
        printf("checking the existance of %s\n", tmp_buff);
#endif

        if(stat(tmp_buff, &sbuf) < 0){
          if(errno != ENOENT){
            perror("stat");
            exit(1);
          }

        } else if(S_ISDIR(sbuf.st_mode)){
#ifdef DEBUG    
          printf("Directory exists\n");
#endif
          continue;
        }else {
          fprintf(stderr, "Hmmm.. %s exists but isn't a directory!\n",
                  tmp_buff);
          exit(1);
        }
        
#ifdef DEBUG    
        printf("Making directory..\n");
#endif
        if(mkdir(tmp_buff, 0755) < 0){
          perror("mkdir");
          exit(1);
        }
        if(verbose && handle)
          printf("%10s: %s/\n", "created", tmp_buff);

      }

      /* only a directory */
      if(strlen((const char *)start) == 0)
        dir = TRUE;

#ifdef DEBUG    
      printf("Leftovers are \"%s\" (%d)\n", start, strlen((const char *)start));
#endif

      /* If the entry was just a directory, don't write to file, etc */
      if(strlen((const char *)start) == 0)
        f_fd = -1;

      free(tmp_buff);
    }

    if(f_fd != -1 && handle){
      f_fd = open((const char *)filename,
                  O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0644);

      if(f_fd < 0){
        fprintf(stderr, "Error extracting JAR archive!\n");
        perror((const char *)filename);
        exit(1);
      }
    }

    if(method != 8 && flags & 0x0008){
      fprintf(stderr, "Error in JAR file! (not compressed but data desc.)\n");
      exit(1);
    }

    if (eflen > 0)
      consume(&pbf, eflen);

    if(method == 8 || flags & 0x0008){
      
      inflate_file(&pbf, f_fd, &ze);
    } else {

#ifdef DEBUG    
      printf("writing stored data.. (%d bytes)\n", csize);
#endif

      out_a = 0;
      in_a = csize;

      ze.crc = crc32(ze.crc, NULL, 0); /* initialize the crc */

      while(out_a < (int)csize){
        rdamt = (in_a > RDSZ ? RDSZ : in_a);
        if(pb_read(&pbf, rd_buff, rdamt) != rdamt){
          perror("read");
          exit(1);
        }
        
        ze.crc = crc32(ze.crc, (Bytef*)rd_buff, rdamt);

        if(f_fd >= 0)
          write(f_fd, rd_buff, rdamt);

        out_a += rdamt;
        in_a -= rdamt;

#ifdef DEBUG    
        printf("%d bytes written\n", out_a);
#endif
      }
    }

    /* if there is a data descriptor left, compare the CRC */
    if(flags & 0x0008){

      if(pb_read(&pbf, scratch, 16) != 16){
        perror("read");
        exit(1);
      }

      signature = UNPACK_UB4(scratch, 0);

      if(signature != 0x08074b50){
        fprintf(stderr, "Error! Missing data descriptor!\n");
        exit(1);
      }

      crc = UNPACK_UB4(scratch, 4);

    }

    if(crc != ze.crc){
      fprintf(stderr, "Error! CRCs do not match! Got %x, expected %x\n",
              ze.crc, crc);
      exit(1);
    }

    close(f_fd);

    if(verbose && dir == FALSE && handle)
      printf("%10s: %s\n",
             (method == 8 ? "inflated" : "extracted"),
             filename);
  }

  return 0;
}

int list_jar(int fd, char **files, int file_num){
  ub4 signature;
  ub4 csize;
  ub4 usize;
  ub4 mdate;
  ub4 tmp;
  ub2 fnlen;
  ub2 eflen;
  ub2 clen;
  ub2 flags;
  ub2 method;
  ub2 cen_size;
  ub1 *filename = NULL;
  ub1 scratch[16];
  ub1 cen_header[46];
  int filename_len = 0;
  off_t size;
  int i, j;
  time_t tdate;
  struct tm *s_tm;
  char ascii_date[31];
  zipentry ze;

#ifdef DEBUG
  printf("Listing jar file, looking for %d files\n", file_num);
#endif

  /* This should be the start of the central-header-end section */
  if(seekable){
    if(lseek(fd, -22, SEEK_END) == (off_t)-1){
      perror("lseek");
      exit(1);
    }
    
    if(read(fd, &tmp, sizeof(ub4)) != 4){
      perror("read");
      exit(1);
    }

#ifdef WORDS_BIGENDIAN
    tmp = L2BI(tmp);
#endif

    if(tmp != 0x06054b50){
      fprintf(stderr, "Error in JAR file format. zip-style comment?\n");
      exit(1);
    }

    if(lseek(fd, 6, SEEK_CUR) == (off_t)-1){
      perror("lseek");
      exit(1);
    }
  
    if(read(fd, &cen_size, 2) != 2){
      perror("read");
      exit(1);
    }

#ifdef WORDS_BIGENDIAN
    cen_size = L2BS(cen_size);
#endif

    /*   printf("%hu entries in central header\n", cen_size); */

    if(lseek(fd, 4, SEEK_CUR) == (off_t)-1){
      perror("lseek");
      exit(1);
    }

    if(read(fd, &tmp, 4) != 4){
      perror("read");
      exit(1);
    }

#ifdef WORDS_BIGENDIAN
    tmp = L2BI(tmp);
#endif

    /*   printf("Central header offset = %d\n", tmp); */

    if(lseek(fd, tmp, SEEK_SET) != (int)tmp){
      perror("lseek");
      exit(1);
    }

    /* Loop through the entries in the central header */
    for(i = 0; i < cen_size; i++){
    
      if(read(fd, &cen_header, 46) != 46){
        perror("read");
        exit(1);
      }

      signature = UNPACK_UB4(cen_header, 0);
      if(signature != 0x02014b50){
        fprintf(stderr, "Error in JAR file! Cannot locate central header!\n");
        exit(1);
      }

      usize = UNPACK_UB4(cen_header, CEN_USIZE);
      fnlen = UNPACK_UB2(cen_header, CEN_FNLEN);
      eflen = UNPACK_UB2(cen_header, CEN_EFLEN);
      clen = UNPACK_UB2(cen_header, CEN_COMLEN);

      /* If we're providing verbose output, we need to make an ASCII
       * formatted version of the date. */
      if(verbose){
        mdate = UNPACK_UB4(cen_header, CEN_MODTIME);
        tdate = dos2unixtime(mdate);
        s_tm = localtime(&tdate);
        strftime(ascii_date, 30, "%a %b %d %H:%M:%S %Z %Y", s_tm);
        ascii_date[30] = '\0';
      }

      if(filename_len < fnlen + 1){
        if(filename != NULL)
          free(filename);
      
        filename = malloc(sizeof(ub1) * (fnlen + 1));
        filename_len = fnlen + 1;
      }
    
      if(read(fd, filename, fnlen) != fnlen){
        perror("read");
        exit(1);
      }
      filename[fnlen] = '\0';
    
      /* if the user specified a list of files on the command line,
         we'll only display those, otherwise we'll display everything */
      if(file_num > 0){
        for(j = 0; j < file_num; j++)
          if(strcmp(files[j], (const char *)filename) == 0){
            if(verbose)
              printf("%6d %s %s\n", usize, ascii_date, filename);
            else
              printf("%s\n", filename);
            break;
          }
      } else {
        if(verbose)
          printf("%6d %s %s\n", usize, ascii_date, filename);
        else
          printf("%s\n", filename);
      }            
      
      size = eflen + clen;
      if(size > 0){
        if(lseek(fd, size, SEEK_CUR) == (off_t)-1){
          perror("lseek");
          exit(1);
        }
      }
    }
  } else {
    /* the file isn't seekable.. evil! */
    pb_file pbf;

    pb_init(&pbf, fd);

    init_inflation();

    for(;;){
      if(pb_read(&pbf, scratch, 4) != 4){
        perror("read");
        break;
      }
      
      signature = UNPACK_UB4(scratch, 0);
      
#ifdef DEBUG
      printf("signature is %x\n", signature);
#endif
      
      if(signature == 0x08074b50){
#ifdef DEBUG
        printf("skipping data descriptor\n");
#endif
        pb_read(&pbf, scratch, 12);
        continue;
      } else if(signature == 0x02014b50){
#ifdef DEBUG
        printf("Central header reached.. we're all done!\n");
#endif
        break;
      }else if(signature != 0x04034b50){
#ifdef DEBUG
        printf("Ick! %#x\n", signature);
#endif
        break;
      }
      
      if(pb_read(&pbf, (file_header + 4), 26) != 26){
        perror("read");
        break;
      }
      
      csize = UNPACK_UB4(file_header, LOC_CSIZE);
#ifdef DEBUG
      printf("Compressed size is %u\n", csize);
#endif
      
      fnlen = UNPACK_UB2(file_header, LOC_FNLEN);
#ifdef DEBUG
      printf("Filename length is %hu\n", fnlen);
#endif
      
      eflen = UNPACK_UB2(file_header, LOC_EFLEN);
#ifdef DEBUG
      printf("Extra field length is %hu\n", eflen);
#endif
      
      method = UNPACK_UB2(file_header, LOC_COMP);
#ifdef DEBUG
      printf("Compression method is %#hx\n", method);
#endif

      flags = UNPACK_UB2(file_header, LOC_EXTRA);
#ifdef DEBUG
      printf("Flags are %#hx\n", flags);
#endif
      
      usize = UNPACK_UB4(file_header, LOC_USIZE);

      /* If we're providing verbose output, we need to make an ASCII
       * formatted version of the date. */
      if(verbose){
        mdate = UNPACK_UB4(file_header, LOC_MODTIME);
        tdate = dos2unixtime(mdate);
        s_tm = localtime(&tdate);
        strftime(ascii_date, 30, "%a %b %d %H:%M:%S %Z %Y", s_tm);
      }

      if(filename_len < fnlen + 1){
        if(filename != NULL)
          free(filename);
        
        filename = malloc(sizeof(ub1) * (fnlen + 1));
        ascii_date[30] = '\0';
        filename_len = fnlen + 1;
      }
      
      pb_read(&pbf, filename, fnlen);
      filename[fnlen] = '\0';
      
      /* the header is at the end.  In a JAR file, this means that the data
         happens to be compressed.  We have no choice but to inflate the
         data */
      if(flags & 0x0008){

        size = eflen;

        if(size > 0)
          consume(&pbf, size);
        
        if(method == 8){
#ifdef DEBUG
          printf("inflating %s\n", filename);
#endif
          inflate_file(&pbf, -1, &ze);

          usize = ze.usize;
        } else 
          printf("We're shit outta luck!\n");
          
      } else {
        size = csize + (eflen > 0 ? eflen : 0);
        

#ifdef DEBUG
        printf("Skipping %ld bytes\n", (long)size);
#endif

        consume(&pbf, size);
      }
      /* print out the listing */
      if(file_num > 0){
        for(j = 0; j < file_num; j++)
          if(strcmp(files[j], (const char *)filename) == 0){
            if(verbose)
              printf("%6d %s %s\n", usize, ascii_date, filename);
            else
              printf("%s\n", filename);
            break;
          }
      } else {
        if(verbose)
          printf("%6d %s %s\n", usize, ascii_date, filename);
        else
          printf("%s\n", filename);
      }        
    }
  }
  return 0;
}

int consume(pb_file *pbf, int amt){
  int tc = 0; /* total amount consumed */
  ub1 buff[RDSZ];
  int rdamt;

#ifdef DEBUG
  printf("Consuming %d bytes\n", amt);
#endif

  if (seekable){
    if (amt <= (int)pbf->buff_amt)
      pb_read(pbf, buff, amt);
    else {
      lseek(pbf->fd, amt - pbf->buff_amt, SEEK_CUR);
      pb_read(pbf, buff, pbf->buff_amt); /* clear pbf */
    }
  } else
  while(tc < amt){
    rdamt = pb_read(pbf, buff, ((amt - tc) < RDSZ ? (amt - tc) : RDSZ));
#ifdef DEBUG
    printf("got %d bytes\n", rdamt);
#endif
    tc += rdamt;
  }

#ifdef DEBUG
  printf("%d bytes consumed\n", amt);
#endif

  return 0;
}

void usage(const char *filename){
  fprintf(stderr, "Try `%s --help' for more information.\n", filename);
  exit (1);
}

void version ()
{
  printf("jar (%s) %s\n\n", PACKAGE, VERSION);
  printf("Copyright 1999, 2000, 2001  Bryan Burns\n");
  printf("Copyright 2002, 2004 Free Software Foundation\n");
  printf("\
This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n");
  exit (0);
}

void help(const char *filename)
{
  printf("\
Usage: %s {ctxuV}[vfm0ME@] [jar-file] [manifest-file] [-C dir] files ...\n\
\n\
Store many files together in a single `jar' file.\n\
\n\
  -c              create new archive\n\
  -t              list table of contents for archive\n\
  -x              extract named (or all) files from archive\n\
  -u              update existing archive\n\
", filename);
  printf("\n\
  -@              read names from stdin\n\
  -0              store only; use no ZIP compression\n\
  -C DIR FILE     change to the specified directory and include\n\
                  the following file\n\
  -E              don't include the files found in a directory\n\
  -f FILE         specify archive file name\n\
  --help          print this help, then exit\n\
  -m FILE         include manifest information from specified manifest file\n\
  -M              Do not create a manifest file for the entries\n\
  -v              generate verbose output on standard output\n\
  -V, --version   display version information\n\
");
  printf("\n\
If any file is a directory then it is processed recursively.\n\
The manifest file name and the archive file name needs to be specified\n\
in the same order the 'm' and 'f' flags are specified.\n\
\n\
Example 1: to archive two class files into an archive called classes.jar: \n\
     jar cvf classes.jar Foo.class Bar.class \n\
Example 2: use an existing manifest file 'mymanifest' and archive all the\n\
     files in the foo/ directory into 'classes.jar': \n\
     jar cvfm classes.jar mymanifest -C foo/ .\n\
");

  exit(0);
}

static char *
jt_strdup(s)
     char *s;
{
  char *result = (char*)malloc(strlen(s) + 1);
  if (result == (char*)0)
    return (char*)0;
  strcpy(result, s);
  return result;
}

/* Convert "tar-style" first argument to a form expected by getopt.
   This idea and the code comes from GNU tar.  This can allocate a new
   argument vector.  This might leak some memory, but we don't care.  */
static void
expand_options (int *argcp, char ***argvp)
{
  int argc = *argcp;
  char **argv = *argvp;

  /* Accept arguments with a leading "-" (eg "-cvf"), but don't do expansion 
     if a long argument (like "--help") is detected. */
  if (argc > 1 && argv[1][1] != '-')
    {
      char buf[3];
      char **new_argv;
      int new_argc;
      int args_to_expand;
      char *p;
      char **in, **out;

      buf[0] = '-';
      buf[2] = '\0';

      args_to_expand = strlen (argv[1]);
      if (argv[1][0] == '-')
        --args_to_expand;
        
      new_argc = argc - 1 + args_to_expand;
      new_argv = (char **) malloc (new_argc * sizeof (char *));
      in = argv;
      out = new_argv;

      *out++ = *in++;
      p = *in++;
      if (*p == '-')
        p++;
      while (*p != '\0')
	{
	  char *opt;
	  buf[1] = *p;
	  *out++ = jt_strdup (buf);
	  /* If the option takes an argument, move the next argument
	     to just after this option.  */
	  opt = strchr (OPTION_STRING, *p);
	  if (opt && opt[1] == ':')
	    {
	      if (in < argv + argc)
		*out++ = *in++;
	      else
		{
		  fprintf(stderr, "%s: option `%s' requires an argument.\n",
			  argv[0], buf);
		  usage(argv[0]);
		}
	    }
	  ++p;
	}

      /* Copy remaining options.  */
      while (in < argv + argc)
	*out++ = *in++;

      *argcp = new_argc;
      *argvp = new_argv;
    }
}
