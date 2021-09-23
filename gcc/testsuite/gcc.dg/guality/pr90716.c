/* { dg-do run } */
/* { dg-options "-g" } */

void __attribute__((noinline))
optimize_me_not ()
{
  __asm__ volatile ("" : : : "memory");
}
int a[7][8];
int main()
{
  int b, j;
  b = 0;
  for (; b < 7; b++) {
      j = 0;
      for (; j < 8; j++)
	a[b][j] = 0;
  }
  /* j may very well be optimized out, so we cannot test for j == 8.
     Instead test j + 1 which will make the test UNSUPPORTED if i
     is optimized out.  Since the test previously had wrong debug
     with j == 0 this is acceptable.  */
  optimize_me_not(); /* { dg-final { gdb-test . "j + 1" "9" { xfail { aarch64*-*-* && { any-opts "-fno-fat-lto-objects" } } } } } */
  return 0;
}
