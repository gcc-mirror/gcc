/* Test whether buffer overflow warnings for __strncat_chk builtin
   are emitted properly.  */
/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu99" } */

extern void abort (void);

#include "../gcc.c-torture/execute/builtins/chk.h"

char buf1[20];
char *q;

void
test (int arg, ...)
{
  char *p = &buf1[10];

  *p = 0;
  strncat (p, "abcdefg", 9);
  *p = 0;
  strncat (p, "abcdefghi", 9);
  *p = 0;
  strncat (p, "abcdefghij", 9);
  *p = 0;
  strncat (p, "abcdefghi", 10);
  *p = 0;
  strncat (p, "abcdefghij", 10); /* { dg-warning "will always overflow" } */
  *p = 0;
  strncat (p, "abcdefgh", 11);
  *p = 0;
  strncat (p, "abcdefghijkl", 11); /* { dg-warning "will always overflow" } */
  *p = 0;
  strncat (p, q, 9);
  *p = 0;
  strncat (p, q, 10); /* { dg-warning "might overflow" } */
  *p = 0;
  strncat (p, q, 11); /* { dg-warning "might overflow" } */
}
