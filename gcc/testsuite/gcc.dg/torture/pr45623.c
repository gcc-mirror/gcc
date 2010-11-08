/* { dg-do run } */
/* { dg-options "-std=c99" } */

#include <stdint.h>

extern void abort (void);

char *s1 = "foo";
char *s2 = "bar";

char **ss1 = &s1;

typedef union jsval_layout
{
    uint64_t asBits;
    char **ptr;
} jsval_layout;

int main()
{
  jsval_layout l, m;
  l.ptr = ss1;
  m.asBits = l.asBits;
  char ** data = m.ptr;
  *data = s2;
  if (s1 != s2)
    abort ();
  return 0;
}
