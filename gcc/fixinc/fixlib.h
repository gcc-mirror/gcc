
/* Install modified versions of certain ANSI-incompatible system header
   files which are fixed to work correctly with ANSI C and placed in a
   directory that GNU C will search.

   Copyright (C) 1997-1999, 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef FIXINCLUDES_FIXLIB_H
#define FIXINCLUDES_FIXLIB_H

#include "auto-host.h"
#include "gansidecl.h"
#include "system.h"

#include "gnu-regex.h"

#ifndef STDIN_FILENO
# define STDIN_FILENO   0
#endif
#ifndef STDOUT_FILENO
# define STDOUT_FILENO  1
#endif

typedef int t_success;

#define FAILURE         (-1)
#define SUCCESS           0
#define PROBLEM           1

#define SUCCEEDED(p)    ((p) == SUCCESS)
#define SUCCESSFUL(p)   SUCCEEDED (p)
#define FAILED(p)       ((p) < SUCCESS)
#define HADGLITCH(p)    ((p) > SUCCESS)


#define tSCC static const char
#define tCC  const char
#define tSC  static char

/* If this particular system's header files define the macro `MAXPATHLEN',
   we happily take advantage of it; otherwise we use a value which ought
   to be large enough.  */
#ifndef MAXPATHLEN
# define MAXPATHLEN     4096
#endif

#ifndef EXIT_SUCCESS
# define EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
# define EXIT_FAILURE 1
#endif

#define NUL             '\0'

#ifndef NOPROCESS
#define NOPROCESS	((pid_t) -1)
#define NULLPROCESS	((pid_t)0)

#define EXIT_PANIC	99

#ifndef HAVE_T_BOOL_ENUM
#define HAVE_T_BOOL_ENUM
typedef enum
{
  BOOL_FALSE, BOOL_TRUE
} t_bool;
#endif

typedef int apply_fix_p_t;  /* Apply Fix Predicate Type */

#define APPLY_FIX 0
#define SKIP_FIX  1

#define _P_(p)	()
#endif

/*
 *  Exported procedures
 */
char * load_file_data _P_(( FILE* fp ));
t_bool is_cxx_header  _P_(( tCC* filename, tCC* filetext ));
void   compile_re     _P_(( tCC* pat, regex_t* re, int match,
			    tCC *e1, tCC *e2 ));
#endif /* FIXINCLUDES_FIXLIB_H */
