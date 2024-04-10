/* { dg-do run } */
/* { dg-additional-options "-ftree-loop-if-convert -fno-vect-cost-model" } */

signed char a[] = {0x80, 0x80,0x80,0x80};
int b;
signed char c;

int main()
{
  for (; b < sizeof(a); b += 1)
    c = a[b] < 0 ?: a[b] >> 6;

  if (c != 1)
    __builtin_abort ();
  return 0;
}
