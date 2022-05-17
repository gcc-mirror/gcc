/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

extern char str[];

unsigned int foo()
{
  __builtin_memset(str,'x',5);
  str[5] = 0;
  return __builtin_strlen (str);
}

/* { dg-final { scan-tree-dump-not "strlen" "optimized" } } */
