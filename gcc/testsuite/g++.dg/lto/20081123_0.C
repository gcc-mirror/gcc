// { dg-lto-do link }
// { dg-require-effective-target fpic }
// { dg-lto-options {{-flto -flto-partition=1to1 -r -nostdlib -fPIC}} }

int
f(void)
{
  return 0;
}
