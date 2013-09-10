/* { dg-do run } */
/* { dg-additional-options "-mtune=pentiumpro" { target ia32 } } */
/* { dg-additional-options "-minline-all-stringops" { target { i?86-*-* x86_64-*-* } } } */

static void __attribute__((noinline, noclone))
my_memcpy (char *dest, const char *src, int n)
{
  __builtin_memcpy (dest, src, n);
}

int
main (void)
{
  char a1[4], a2[4];
  __builtin_memset (a1, 'a', 4);
  __builtin_memset (a2, 'b', 4);
  my_memcpy (a2, a1, 4);
  if (a2[0] != 'a')
    __builtin_abort ();
  return 0;
}

