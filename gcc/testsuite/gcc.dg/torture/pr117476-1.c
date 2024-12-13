/* PR rtl-optimization/117476.
   First case checking out of mode N non-zero bits. */
/* { dg-do run } */

int c = 0x1FF;

int main()
{
  if (((c ^ 0xFF) & 0xFF) != 0)
    __builtin_abort();
  return 0;
}
