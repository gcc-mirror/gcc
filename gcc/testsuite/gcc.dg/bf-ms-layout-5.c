/* PR target/52991 */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */

struct S {
  int a : 2;
  __attribute__((aligned (8))) int b : 2;
  int c : 28;
  __attribute__((aligned (16))) int d : 2;
  int e : 30;
} __attribute__((ms_struct));

struct S s;

int
main ()
{
  int i;
  if (sizeof (s) != 32)
    __builtin_abort ();
  s.a = -1;
  for (i = 0; i < 32; ++i)
    if (((char *) &s)[i] != (i ? 0 : 3))
      __builtin_abort ();
  s.a = 0;
  s.b = -1;
  for (i = 0; i < 32; ++i)
    if (((char *) &s)[i] != (i ? 0 : 12))
      __builtin_abort ();
  s.b = 0;
  s.c = -1;
  for (i = 0; i < 32; ++i)
    if (((signed char *) &s)[i] != (i > 3 ? 0 : (i ? -1 : -16)))
      __builtin_abort ();
  s.c = 0;
  s.d = -1;
  for (i = 0; i < 32; ++i)
    if (((signed char *) &s)[i] != (i == 16 ? 3 : 0))
      __builtin_abort ();
  s.d = 0;
  s.e = -1;
  for (i = 0; i < 32; ++i)
    if (((signed char *) &s)[i] != ((i < 16 || i > 19) ? 0 : (i == 16 ? -4 : -1)))
      __builtin_abort ();
  return 0;
}
