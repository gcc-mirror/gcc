/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* This tests correct spacing of macro expansion output, as well as
   the line it falls on.  This is quite subtle; it involves newlines
   within macro arguments becoming spaces, but not if it turns out to
   not be a macro invocation.  Also, multiple macro invocations spread
   across many lines.

   Neil Booth, 1 Dec 2000.  */

#define str(x) #x
#define f(x) x

/* The correct output is shown here.  Note the spaces, and the way
   everything after the invocation of f appears on the same line.

f
bar
g "1 2" bam baz

*/

f
bar
f (g) str
(
1
2
) f
(bam) baz

/*
   { dg-final { if ![file exists spacing1.i] { return }                   } }
   { dg-final { if \{ [grep spacing1.i "f.*bar"] == "" \} \{              } }
   { dg-final { if \{ [grep spacing1.i "^bar"] != "" \}   \{              } }
   { dg-final { if \{ [grep spacing1.i "g \"1 2\" bam baz"] != "" \} \{   } }
   { dg-final { return \} \} \}                                           } }
   { dg-final { fail "spacing1.c: spacing and new-line preservation"      } }
*/
