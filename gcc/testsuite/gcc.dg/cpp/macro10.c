/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Source: Neil Booth, 23 Sep 2001.

   A tricky, pathological corner case we used to get wrong.  Expansion
   should go as follows.  The asterisk indicates the token has "blue
   paint" can no longer be macro expanded.  We used to lose that
   information when parsing arguments and dropping to the lexer to get
   the ')'.

   foo )
   bar foo* )
   func (foo* )
   foo*   

   If we try and expand the final foo, we get an "unterminated
   argument list invoking macro <func>" error.  If we do the right
   thing and leave it as is, no diagnostics are emitted.  */

#define func(x) x
#define bar func(
#define foo bar foo
foo )
