/* PR c/117456 */
/* { dg-do run } */
/* { dg-options "-std=c11" } */

struct S {
  unsigned s : 5;
};

unsigned
f1 (struct S s)
{
  return __builtin_stdc_rotate_left (s.s, 3);
}

unsigned
f2 (struct S s, int n)
{
  return __builtin_stdc_rotate_left (s.s, n);
}

unsigned
f3 (struct S s)
{
  return __builtin_stdc_rotate_right (s.s, 2);
}

unsigned
f4 (struct S s, int n)
{
  return __builtin_stdc_rotate_right (s.s, n);
}

#if __BITINT_MAXWIDTH__ >= 64
unsigned _BitInt(5)
f5 (unsigned _BitInt(5) s)
{
  return __builtin_stdc_rotate_left (s, 3);
}

unsigned _BitInt(5)
f6 (unsigned _BitInt(5) s, int n)
{
  return __builtin_stdc_rotate_left (s, n);
}

unsigned _BitInt(5)
f7 (unsigned _BitInt(5) s)
{
  return __builtin_stdc_rotate_right (s, 2);
}

unsigned _BitInt(5)
f8 (unsigned _BitInt(5) s, int n)
{
  return __builtin_stdc_rotate_right (s, n);
}
#endif

#if __BITINT_MAXWIDTH__ >= 125
unsigned _BitInt(125)
f9 (unsigned _BitInt(125) s)
{
  return __builtin_stdc_rotate_left (s, 13);
}

unsigned _BitInt(125)
f10 (unsigned _BitInt(125) s, int n)
{
  return __builtin_stdc_rotate_left (s, n);
}

unsigned _BitInt(125)
f11 (unsigned _BitInt(125) s)
{
  return __builtin_stdc_rotate_right (s, 42);
}

unsigned _BitInt(125)
f12 (unsigned _BitInt(125) s, int n)
{
  return __builtin_stdc_rotate_right (s, n);
}
#endif

int
main ()
{
  struct S s = { 0x12 };
  if (f1 (s) != 0x14
      || f2 (s, 0) != 0x12
      || f2 (s, 2) != 0xa
      || f2 (s, 1) != 0x5
      || f3 (s) != 0x14
      || f4 (s, 0) != 0x12
      || f4 (s, 2) != 0x14
      || f4 (s, 1) != 0x9)
    __builtin_abort ();
#if __BITINT_MAXWIDTH__ >= 64
  if (f5 (0x12uwb) != 0x14uwb
      || f6 (0x12uwb, 0) != 0x12uwb
      || f6 (0x12uwb, 2) != 0xauwb
      || f6 (0x12uwb, 1) != 0x5uwb
      || f7 (0x12uwb) != 0x14uwb
      || f8 (0x12uwb, 0) != 0x12uwb
      || f8 (0x12uwb, 2) != 0x14uwb
      || f8 (0x12uwb, 1) != 0x9uwb)
    __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 125
  if (f9 (12107255122146692213464668179507246062uwb) != 32859299037257821061785486091897129243uwb
      || f10 (12107255122146692213464668179507246062uwb, 0) != 12107255122146692213464668179507246062uwb
      || f10 (12107255122146692213464668179507246062uwb, 57) != 786310737972746809290227161460052307uwb
      || f10 (12107255122146692213464668179507246062uwb, 1) != 24214510244293384426929336359014492124uwb
      || f11 (12107255122146692213464668179507246062uwb) != 25567301336572975565218391744704605699uwb
      || f12 (12107255122146692213464668179507246062uwb, 0) != 12107255122146692213464668179507246062uwb
      || f12 (12107255122146692213464668179507246062uwb, 22) != 27217840477347696606051931660144451082uwb
      || f12 (12107255122146692213464668179507246062uwb, 1) != 6053627561073346106732334089753623031uwb)
    __builtin_abort ();
#endif
}
