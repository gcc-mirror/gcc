/* Test for flexible array members.  Test for agreement of offset and
   structure size.  This is expected to fail, because of a possible
   defect in the standard.  */
/* Origin: http://gcc.gnu.org/ml/gcc/2002-05/msg02844.html
   from Tony Finch <dot@dotat.at>, adapted to a testcase by Joseph Myers
   <jsm28@cam.ac.uk>.  See also WG14 reflector messages 9571-3.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

#include <stddef.h>

struct foo {
  int a;
  short b;
  char pad[];
};

struct bar {
  int a;
  short b;
  char pad[1024];
};

char x[(sizeof(struct foo) == offsetof(struct foo, pad)) ? 1 : -1]; /* { dg-bogus "negative" "sizeof != offsetof" { xfail *-*-* } } */
char y[(offsetof(struct foo, pad) == offsetof(struct bar, pad)) ? 1 : -1];
