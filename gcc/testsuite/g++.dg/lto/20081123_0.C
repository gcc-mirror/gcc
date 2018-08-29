// { dg-lto-do link }
// { dg-require-effective-target fpic }
// { dg-lto-options {{-flto -flto-partition=1to1 -r -nostdlib -fPIC}} }
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */

int
f(void)
{
  return 0;
}
