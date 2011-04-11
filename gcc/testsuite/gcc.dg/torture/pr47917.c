/* { dg-do run } */
/* { dg-options "-std=c99" } */
/* { dg-options "-std=c99 -D_ISO_C_SOURCE=19990L" { target alpha*-dec-osf5* } } */
/* { dg-options "-std=c99 -D_XOPEN_SOURCE=500" { target mips-sgi-irix6.5 *-*-solaris2.[89] } } */
/* { dg-options "-std=gnu99" { target *-*-hpux* } } */
/* { dg-xfail-if "no C99 snprintf function" { *-*-hpux10* } } */
/* { dg-xfail-run-if "non-conforming C99 snprintf" { *-*-hpux11.[012]* } } */

/* PR middle-end/47917 */

#include <stdio.h>
extern int memcmp (const void *, const void *, __SIZE_TYPE__);
extern void abort (void);

char buf1[6], buf2[6], buf3[4], buf4[4];
int i;

int
foo (void)
{
  int ret = snprintf (buf1, sizeof buf1, "abcde");
  ret += snprintf (buf2, sizeof buf2, "abcdef") * 16;
  ret += snprintf (buf3, sizeof buf3, "%s", i++ < 6 ? "abc" : "def") * 256;
  ret += snprintf (buf4, sizeof buf4, "%s", i++ > 10 ? "abcde" : "defgh") * 4096;
  return ret;
}

int
main (void)
{
  if (foo () != 5 + 6 * 16 + 3 * 256 + 5 * 4096)
    abort ();
  if (memcmp (buf1, "abcde", 6) != 0
      || memcmp (buf2, "abcde", 6) != 0
      || memcmp (buf3, "abc", 4) != 0
      || memcmp (buf4, "def", 4) != 0
      || i != 2)
    abort ();
  return 0;
}
