/* { dg-lto-do link } */
/* { dg-lto-options { { -O2 -flto -flto-partition=1to1 -fno-early-inlining -fno-ipa-sra -w } } } */

extern void __attribute__((noinline)) entry(void);

int
main (int argc, char **argv)
{
  entry();
  return 0;
}
