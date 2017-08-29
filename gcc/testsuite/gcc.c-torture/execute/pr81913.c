/* PR tree-optimization/81913 */

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
  u32 x = b (1, -0x378704, ~0xba64fc);
  if (x != 0xd93190d0)
    __builtin_abort ();
  return 0;
}

