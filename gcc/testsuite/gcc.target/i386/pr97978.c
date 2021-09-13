/* { dg-do compile } */
/* { dg-options "-Os -fno-PIC" } */
int sg;
long int kk;

void
bp (int jz, int tj, long int li)
{
  if (jz == 0 || tj == 0)
    __builtin_unreachable ();

  kk = li;
}

void
qp (void)
{
  ++kk;

  for (;;)
    bp (1l / sg, 0, ~0u);
}
