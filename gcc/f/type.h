/* Interface definitions for Fortran type abstraction
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
02111-1307, USA.  */

#ifndef _H_f_type
#define _H_f_type

typedef struct _ffetype_ *ffetype;
typedef struct _ffetype_indexes_ *ffetype_indexes_;

struct _ffetype_
  {
    ffetype_indexes_ kinds_;
    ffetype_indexes_ stars_;
    int alignment_;
    int modulo_;
    int size_;
  };

struct _ffetype_indexes_
  {
    ffetype type_[40];	/* *n, KIND=n: 0 <= n <= 39. */
  };

#define ffetype_alignment(t) ((t)->alignment_)
#define ffetype_init_0()
#define ffetype_init_1()
#define ffetype_init_2()
#define ffetype_init_3()
#define ffetype_init_4()
ffetype ffetype_lookup_kind (ffetype base_type, int kind);
ffetype ffetype_lookup_star (ffetype base_type, int star);
#define ffetype_modulo(t) ((t)->modulo_)
ffetype ffetype_new (void);
#define ffetype_set_ams(t,a,m,s) ((t)->alignment_ = (a), \
				  (t)->modulo_ = (m), \
				  (t)->size_ = (s))
void ffetype_set_kind (ffetype base_type, int kind, ffetype type);
void ffetype_set_star (ffetype base_type, int star, ffetype type);
#define ffetype_size(t) ((t)->size_)
#define ffetype_terminate_0()
#define ffetype_terminate_1()
#define ffetype_terminate_2()
#define ffetype_terminate_3()
#define ffetype_terminate_4()

#endif
