/* PR middle-end/47917 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

extern int snprintf (char *, __SIZE_TYPE__, const char *, ...);
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

/* { dg-final { scan-tree-dump-times "snprintf" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "sprintf" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
