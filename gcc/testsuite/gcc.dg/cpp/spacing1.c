/* Copyright (C) 2000, 2001 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* This tests correct spacing of macro expansion output, as well as
   the line it falls on.  This is quite subtle; it involves newlines
   within macro arguments becoming spaces, but not if it turns out to
   not be a macro invocation.  Also, multiple macro invocations spread
   across many lines.

   Neil Booth, 1 Dec 2000, 23 Sep 2001.  */

#define str(x) #x
#define f(x) x
#define glue(x, y) x ## y
#define EMPTY
/* These are based on PR 4492, we mustn't lose padding tokens when
   scanning ahead for a '(' and failing to find it.  */
#define A(x) B x
#define B(x)
#define C A
#define D() A

/* The correct output is shown here.  Note the spaces, and the way
   everything after the invocation of f appears on the same line.

 44 ;
B Q B Q A Q A:
f
bar
g "1 2" bam baz

*/

glue (EMPTY 4, 4) EMPTY;
A(Q) C(Q) D()Q D():
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
   { dg-final { if \{ [grep spacing1.i " 44 ;"] != "" \}              \{  } }
   { dg-final { if \{ [grep spacing1.i "B Q B Q A Q A:"] != "" \}     \{  } }
   { dg-final { if \{ [grep spacing1.i "f.*bar"] == "" \} \{              } }
   { dg-final { if \{ [grep spacing1.i "^bar"] != "" \}   \{              } }
   { dg-final { if \{ [grep spacing1.i "g \"1 2\" bam baz"] != "" \} \{   } }
   { dg-final { return \} \} \} \} \}                                     } }
   { dg-final { fail "spacing1.c: spacing and new-line preservation"      } }
*/
