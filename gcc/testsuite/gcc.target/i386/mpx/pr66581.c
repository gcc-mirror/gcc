/* { dg-do compile } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */

void *a;
int b;

void
fn1 (void)
{
  void *c = &&l_nop;
l_nop:
    for (; b;)
    ;
  int *d = c;
  c = fn1;
  *d = 1;
  goto *a;
}
