/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* This tests that we avoid accidental pasting, as well as gratuitous
   space insertion, in various nasty places _inside_ a macro's
   replacement list: on either side of a paste, and on either side of
   an argument.  It also tests that we don't pass empty macro leading
   whitespace to the next line - this problem use to break Emacs
   preprocessor abuse.

   Neil Booth, 1 Feb 2001.  */

#define EMPTY_WITH_LEADING_SPACE
#define f(x, y) :x: -y##> -##y>
#define g(x, y) :x: :y##2 2##y:

/* This should preprocess as

: : : - > - >
:2: :22 22:

We used to get a space at the start of the line.  */

 EMPTY_WITH_LEADING_SPACE
f(:,) 
g(2, 2)

/*
   { dg-final { if ![file exists avoidpaste2.i] { return }                } }
   { dg-final { if { [grep avoidpaste2.i "^: : : - > - >"] != "" } \{     } }
   { dg-final { if { [grep avoidpaste2.i "^:2: :22 22:"] != "" }   \{     } }
   { dg-final { return \} \}                                              } }
   { dg-final { fail "avoidpaste2.c: paste avoidance"                     } }
*/
