/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-additional-options "-fstrict-aliasing -fdump-tree-fre1" } */

float f;
int foo(int *p, int *q)
{
  *p = 0;
  if (*p)
    *q = 1;
  else
    f = 8.0f;
  return *p;
}

/* { dg-final { scan-tree-dump "return 0;" "fre1" } } */
