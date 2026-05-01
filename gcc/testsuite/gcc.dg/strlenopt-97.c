/* PR tree-optimization/125079 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-strlen1" } */
/* { dg-final { scan-tree-dump-times "__builtin___strcpy_chk \\\(" 1 "strlen1" } } */
/* { dg-final { scan-tree-dump-not "__builtin___strcpy_chk \\\(\[^\n\r]*, 64\\\)" "strlen1" } } */
/* { dg-final { scan-tree-dump-times "__builtin___stpcpy_chk \\\(" 1 "strlen1" } } */
/* { dg-final { scan-tree-dump-not "__builtin___stpcpy_chk \\\(\[^\n\r]*, 128\\\)" "strlen1" } } */

typedef __SIZE_TYPE__ size_t;
void foo (char *, int);
char *stpcpy (char *, const char *);

size_t
bar (char *r)
{
  char buf[64];
  foo (buf, 0);
  size_t ret = __builtin_strlen (buf);
  __builtin___strcat_chk (buf, r, 64);
  foo (buf, 1);
  return ret;
}

size_t
baz (char *r)
{
  char buf[128];
  foo (buf, 2);
  __builtin___strcat_chk (buf, r, 128);
  size_t ret = __builtin_strlen (buf);
  foo (buf, 3);
  return ret;
}
