/* { dg-do compile } */
/* { dg-options "-O2 -fno-code-hoisting -fdump-tree-pre-stats" } */

int f();
int d;
void c(int x)
{
  int (*fp)() __attribute__((const)) = (void *)f;
  if (x)
    d = fp ();
  int tem = fp ();
  f();
  d = tem;
}

/* We shouldn't ICE and PRE the const call.  */
/* { dg-final { scan-tree-dump "Eliminated: 1" "pre" } } */
