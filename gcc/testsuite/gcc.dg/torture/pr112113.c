/* { dg-do run } */
/* The -Waggressive-loop-optimizations diagnostic is spurious, missed
   constant propagation after final value replacement.  */
/* { dg-additional-options "-Wno-aggressive-loop-optimizations -fsplit-loops" } */

volatile int a;
int main()
{
  for (int b = 0; b < 33; b += 3) {
    if (b > 31)
      a++;
  }
  if (a != 0)
    __builtin_abort();
  return 0;
}
