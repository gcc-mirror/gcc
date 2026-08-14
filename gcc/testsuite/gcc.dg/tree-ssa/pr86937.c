/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */
const char a[4] = "123";

int f (int i)
{
  return __builtin_strlen (i ? a : "");
}

int g (int i)
{
  return __builtin_strnlen (i ? a : "", 4);
}

/* { dg-final { scan-tree-dump-times "strnlen " 0 "pre" } } */
