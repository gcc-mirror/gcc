/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-sccp-details" } */

typedef unsigned char u8;
typedef unsigned int u32;

static u32
b (u8 d, u32 e, u32 g)
{
  do
    {
      e += g + 1;
      d--;
    }
  while (d >= (u8) e);

  return e;
}

int
main (void)
{
  u32 x = b (200, -0x378704, ~0xba64fc);
  if (x != 0xe1ee4ca0)
    __builtin_abort ();

  return 0;
}

/* Niter analyzer should be able to compute niters for the loop.  */
/* { dg-final { scan-tree-dump "Replacing uses of: .* with: 3790490784" "sccp" { xfail *-*-* } } } */
