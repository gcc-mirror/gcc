/* { dg-do run } */
/* { dg-options "" } */
/* { dg-options "-O0 -mtune=i386 -fomit-frame-pointer" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

__attribute__((noreturn,noinline)) void abrt (const char *fi, const char *fu)
{
  __builtin_abort ();
}

__attribute__((noinline)) int f (int k)
{
  return k;
}

__attribute__((noinline)) int g (int t, int k)
{
  int b;

  switch (t)
    {
    case 0:
      abrt (__FILE__, __FUNCTION__);

    case 1:
      b = f (k);
      break;

    case 2:
      b = f (k);
      break;

    case 3:
      b = f (k);
      break;

    case 4:
      b = f (k);
      break;

    default:
      abrt (__FILE__, __FUNCTION__);
    }

  return b;
}

int main (void)
{
  if (g (3, 1337) != 1337)
      abrt (__FILE__, __FUNCTION__);
  return 0;
}
