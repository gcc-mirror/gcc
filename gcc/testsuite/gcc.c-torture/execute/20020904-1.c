/* PR c/7102 */

/* Verify that GCC zero-extends integer constants
   in unsigned binary operations. */

typedef unsigned char u8;

u8 fun(u8 y)
{
  u8 x=((u8)255)/y;
  return x;
}

int main(void)
{
  if (fun((u8)2) != 127)
    abort ();
  return 0;
}
