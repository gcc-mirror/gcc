/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void *memset (void *, int, __SIZE_TYPE__);
extern __SIZE_TYPE__ strlen (const char *);

int
main (int argc, char **argv)
{
  char x[8] = "abc";
  memset (x, argc, strlen (x));
  return 0;
}
/* { dg-final { scan-assembler-not "(?n)strlen\(.*\n\)+.*strlen" { xfail *-*-darwin* } } } */
