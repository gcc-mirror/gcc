/* where.h -- Public #include File (module.h template V1.0)
   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

   Owning Modules:
      where.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_WHERE_H
#define GCC_F_WHERE_H

/* Simple definitions and enumerations. */

#define FFEWHERE_columnMAX UCHAR_MAX
#define FFEWHERE_columnUNKNOWN 0
#define FFEWHERE_indexMAX 36
#define FFEWHERE_indexUNKNOWN UCHAR_MAX
#define FFEWHERE_lineMAX ULONG_MAX
#define FFEWHERE_lineUNKNOWN (&ffewhere_unknown_line_)
#define FFEWHERE_filenameUNKNOWN ("(input file)")

/* Typedefs. */

typedef unsigned char ffewhereColumnNumber;	/* Change FFEWHERE_columnMAX
						   too. */
#define ffewhereColumnNumber_f ""
typedef unsigned char ffewhereColumn;
typedef struct _ffewhere_file_ *ffewhereFile;
typedef unsigned short ffewhereLength_;
#define ffewhereLength_f_ ""
typedef unsigned long ffewhereLineNumber;	/* Change FFEWHERE_lineMAX
						   too. */
#define ffewhereLineNumber_f "l"
typedef struct _ffewhere_line_ *ffewhereLine;
typedef unsigned char ffewhereIndex;
#define ffewhereIndex_f ""
typedef ffewhereIndex ffewhereTrack[FFEWHERE_indexMAX * 2 - 2];
typedef unsigned int ffewhereUses_;
#define ffewhereUses_f_ ""

/* Include files needed by this one. */

#include "glimits.h"
#include "top.h"

/* Structure definitions. */

struct _ffewhere_file_
  {
    size_t length;
    char text[1];
  };

struct _ffewhere_line_
  {
    ffewhereLine next;
    ffewhereLine previous;
    ffewhereLineNumber line_num;
    ffewhereUses_ uses;
    ffewhereLength_ length;
    char content[1];
  };

/* Global objects accessed by users of this module. */

extern struct _ffewhere_line_ ffewhere_unknown_line_;

/* Declare functions with prototypes. */

void ffewhere_file_kill (ffewhereFile wf);
ffewhereFile ffewhere_file_new (const char *name, size_t length);
void ffewhere_file_set (ffewhereFile wf, bool have_num, ffewhereLineNumber ln);
void ffewhere_init_1 (void);
char *ffewhere_line_content (ffewhereLine l);
ffewhereFile ffewhere_line_file (ffewhereLine l);
ffewhereLineNumber ffewhere_line_filelinenum (ffewhereLine l);
void ffewhere_line_kill (ffewhereLine l);
ffewhereLine ffewhere_line_new (ffewhereLineNumber ln);
ffewhereLine ffewhere_line_use (ffewhereLine wl);
void ffewhere_set_from_track (ffewhereLine *wol, ffewhereColumn *woc,
		     ffewhereLine wrl, ffewhereColumn wrc, ffewhereTrack wt,
			      ffewhereIndex i);
void ffewhere_track (ffewhereLine *wl, ffewhereColumn *wc, ffewhereTrack wt,
	   ffewhereIndex i, ffewhereLineNumber ln, ffewhereColumnNumber cn);
void ffewhere_track_clear (ffewhereTrack wt, ffewhereIndex length);
void ffewhere_track_copy (ffewhereTrack dwt, ffewhereTrack swt,
			  ffewhereIndex start, ffewhereIndex length);
void ffewhere_track_kill (ffewhereLine wrl, ffewhereColumn wrc, ffewhereTrack wt,
			  ffewhereIndex length);

/* Define macros. */

#define ffewhere_column_is_unknown(c) (c == FFEWHERE_columnUNKNOWN)
#define ffewhere_column_kill(c) ((void) 0)
#define ffewhere_column_new(cn) (cn)
#define ffewhere_column_number(c) (c)
#define ffewhere_column_unknown() (FFEWHERE_columnUNKNOWN)
#define ffewhere_column_use(c) (c)
#define ffewhere_file_name(f) ((f)->text)
#define ffewhere_file_namelen(f) ((f)->length)
#define ffewhere_init_0()
#define ffewhere_init_2()
#define ffewhere_init_3()
#define ffewhere_init_4()
#define ffewhere_line_filename(l) (ffewhere_line_file(l)->text)
#define ffewhere_line_is_unknown(l) (l == FFEWHERE_lineUNKNOWN)
#define ffewhere_line_number(l) ((l)->line_num)
#define ffewhere_line_unknown() (FFEWHERE_lineUNKNOWN)
#define ffewhere_terminate_0()
#define ffewhere_terminate_1()
#define ffewhere_terminate_2()
#define ffewhere_terminate_3()
#define ffewhere_terminate_4()

/* End of #include file. */

#endif /* ! GCC_F_EHERE_H */
