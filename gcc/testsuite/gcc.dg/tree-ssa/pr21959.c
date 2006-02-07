/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

unsigned char c[0xFF];
void f(void)
{
  unsigned char i;
  c[128] = 128;
  i = 0;
  while (1)
  {
    /* This predicate should not be folded out.  */
    if (((signed char) i) < 0) break;
    c[i] = ' ';
    i++;
  }
}

/* { dg-final { scan-tree-dump-times "Folding predicate " 0 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
