/* { dg-do preprocess } */
/* { dg-options "-fno-show-column" } */

/* Tests for un-terminated conditional diagnostics.
   Copyright (c) 1999 Free Software Foundation.
   Contributed by Zack Weinberg, who made it up all by himself.  */

#endif /* { dg-error "#endif" "#endif without #if" } */

#else  /* { dg-error "#else" "#else without #if" } */
#endif /* { dg-error "#endif" "#endif after #else without #if" } */

#elif 0 /* { dg-error "#elif" "#elif without #if" } */
#endif  /* { dg-error "#endif" "#endif after #else without #if" } */

#if 1  /* { dg-bogus "unterminated" "terminated true conditional" } */
blah
#endif

#if 0  /* { dg-bogus "unterminated" "terminated false conditional" } */
ignored
#endif

/* We shouldn't see unbalanced conditionals inside #if'ed out #includes.  */
#if 0
#include "cpp-unc1.c"
#endif

/* The ifdef below should not get an error just because there's a bad if
   inside the included file.  
   The odd dg-error tag on the include matches the "In file included from"
   message.  */
#define FOO
#ifdef FOO  /* { dg-bogus "unterminated" "nested unterm" } */
#include "cpp-unc1.c"  /* { dg-error "" } */
#endif

/* dg.exp doesn't read the included files for tags, so we have to
   do them explicitly here.  */
/* { dg-error "#if" "unc1.h: unterminated #if" { target native } 3 } */
