/* PR c/111309 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 156
__attribute__((noipa)) int
clz156 (unsigned _BitInt(156) x)
{
  return __builtin_clzg (x);
}

__attribute__((noipa)) int
clzd156 (unsigned _BitInt(156) x)
{
  return __builtin_clzg (x, __builtin_popcountg ((typeof (x)) ~(typeof (x)) 0));
}

__attribute__((noipa)) int
clzD156 (unsigned _BitInt(156) x, int y)
{
  return __builtin_clzg (x, y);
}

__attribute__((noipa)) int
ctz156 (unsigned _BitInt(156) x)
{
  return __builtin_ctzg (x);
}

__attribute__((noipa)) int
ctzd156 (unsigned _BitInt(156) x)
{
  return __builtin_ctzg (x, __builtin_popcountg ((typeof (x)) ~(typeof (x)) 0));
}

__attribute__((noipa)) int
ctzD156 (unsigned _BitInt(156) x, int y)
{
  return __builtin_ctzg (x, y);
}

__attribute__((noipa)) int
clrsb156 (_BitInt(156) x)
{
  return __builtin_clrsbg (x);
}

__attribute__((noipa)) int
ffs156 (_BitInt(156) x)
{
  return __builtin_ffsg (x);
}

__attribute__((noipa)) int
parity156 (unsigned _BitInt(156) x)
{
  return __builtin_parityg (x);
}

__attribute__((noipa)) int
popcount156 (unsigned _BitInt(156) x)
{
  return __builtin_popcountg (x);
}
#endif

#if __BITINT_MAXWIDTH__ >= 192
__attribute__((noipa)) int
clz192 (unsigned _BitInt(192) x)
{
  return __builtin_clzg (x);
}

__attribute__((noipa)) int
clzd192 (unsigned _BitInt(192) x)
{
  return __builtin_clzg (x, __builtin_popcountg ((typeof (x)) ~(typeof (x)) 0));
}

__attribute__((noipa)) int
clzD192 (unsigned _BitInt(192) x, int y)
{
  return __builtin_clzg (x, y);
}

__attribute__((noipa)) int
ctz192 (unsigned _BitInt(192) x)
{
  return __builtin_ctzg (x);
}

__attribute__((noipa)) int
ctzd192 (unsigned _BitInt(192) x)
{
  return __builtin_ctzg (x, __builtin_popcountg ((typeof (x)) ~(typeof (x)) 0));
}

__attribute__((noipa)) int
ctzD192 (unsigned _BitInt(192) x, int y)
{
  return __builtin_ctzg (x, y);
}

__attribute__((noipa)) int
clrsb192 (_BitInt(192) x)
{
  return __builtin_clrsbg (x);
}

__attribute__((noipa)) int
ffs192 (_BitInt(192) x)
{
  return __builtin_ffsg (x);
}

__attribute__((noipa)) int
parity192 (unsigned _BitInt(192) x)
{
  return __builtin_parityg (x);
}

__attribute__((noipa)) int
popcount192 (unsigned _BitInt(192) x)
{
  return __builtin_popcountg (x);
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 156
  if (clzd156 (0) != 156
      || clzD156 (0, -1) != -1
      || ctzd156 (0) != 156
      || ctzD156 (0, 42) != 42
      || clrsb156 (0) != 156 - 1
      || ffs156 (0) != 0
      || parity156 (0) != 0
      || popcount156 (0) != 0
      || __builtin_clzg ((unsigned _BitInt(156)) 0, 156 + 32) != 156 + 32
      || __builtin_ctzg ((unsigned _BitInt(156)) 0, 156) != 156
      || __builtin_clrsbg ((_BitInt(156)) 0) != 156 - 1
      || __builtin_ffsg ((_BitInt(156)) 0) != 0
      || __builtin_parityg ((unsigned _BitInt(156)) 0) != 0
      || __builtin_popcountg ((unsigned _BitInt(156)) 0) != 0)
    __builtin_abort ();
  if (clz156 (-1) != 0
      || clzd156 (-1) != 0
      || clzD156 (-1, 0) != 0
      || ctz156 (-1) != 0
      || ctzd156 (-1) != 0
      || ctzD156 (-1, 17) != 0
      || clrsb156 (-1) != 156 - 1
      || ffs156 (-1) != 1
      || parity156 (-1) != 0
      || popcount156 (-1) != 156
      || __builtin_clzg ((unsigned _BitInt(156)) -1) != 0
      || __builtin_clzg ((unsigned _BitInt(156)) -1, 156 + 32) != 0
      || __builtin_ctzg ((unsigned _BitInt(156)) -1) != 0
      || __builtin_ctzg ((unsigned _BitInt(156)) -1, 156) != 0
      || __builtin_clrsbg ((_BitInt(156)) -1) != 156 - 1
      || __builtin_ffsg ((_BitInt(156)) -1) != 1
      || __builtin_parityg ((unsigned _BitInt(156)) -1) != 0
      || __builtin_popcountg ((unsigned _BitInt(156)) -1) != 156)
    __builtin_abort ();
  if (clz156 (((unsigned _BitInt(156)) -1) >> 24) != 24
      || clz156 (((unsigned _BitInt(156)) -1) >> 79) != 79
      || clz156 (1) != 156 - 1
      || clzd156 (((unsigned _BitInt(156)) -1) >> 139) != 139
      || clzd156 (2) != 156 - 2
      || ctz156 (((unsigned _BitInt(156)) -1) << 42) != 42
      || ctz156 (((unsigned _BitInt(156)) -1) << 57) != 57
      || ctz156 (0x4000000000000000000000uwb) != 86
      || ctzd156 (((unsigned _BitInt(156)) -1) << 149) != 149
      || ctzd156 (2) != 1
      || clrsb156 ((unsigned _BitInt(156 - 4)) -1) != 3
      || clrsb156 ((unsigned _BitInt(156 - 28)) -1) != 27
      || clrsb156 ((unsigned _BitInt(156 - 29)) -1) != 28
      || clrsb156 (~(unsigned _BitInt(156)) (unsigned _BitInt(156 - 68)) -1) != 67
      || clrsb156 (~(unsigned _BitInt(156)) (unsigned _BitInt(156 - 92)) -1) != 91
      || clrsb156 (~(unsigned _BitInt(156)) (unsigned _BitInt(156 - 93)) -1) != 92
      || ffs156 (((unsigned _BitInt(156)) -1) << 42) != 43
      || ffs156 (((unsigned _BitInt(156)) -1) << 57) != 58
      || ffs156 (0x4000000000000000000000uwb) != 87
      || ffs156 (((unsigned _BitInt(156)) -1) << 149) != 150
      || ffs156 (2) != 2
      || __builtin_clzg (((unsigned _BitInt(156)) -1) >> 24) != 24
      || __builtin_clzg (((unsigned _BitInt(156)) -1) >> 79) != 79
      || __builtin_clzg ((unsigned _BitInt(156)) 1) != 156 - 1
      || __builtin_clzg (((unsigned _BitInt(156)) -1) >> 139, 156) != 139
      || __builtin_clzg ((unsigned _BitInt(156)) 2, 156) != 156 - 2
      || __builtin_ctzg (((unsigned _BitInt(156)) -1) << 42) != 42
      || __builtin_ctzg (((unsigned _BitInt(156)) -1) << 57) != 57
      || __builtin_ctzg ((unsigned _BitInt(156)) 0x4000000000000000000000uwb) != 86
      || __builtin_ctzg (((unsigned _BitInt(156)) -1) << 149, 156) != 149
      || __builtin_ctzg ((unsigned _BitInt(156)) 2, 156) != 1
      || __builtin_clrsbg ((_BitInt(156)) (unsigned _BitInt(156 - 4)) -1) != 3
      || __builtin_clrsbg ((_BitInt(156)) (unsigned _BitInt(156 - 28)) -1) != 27
      || __builtin_clrsbg ((_BitInt(156)) (unsigned _BitInt(156 - 29)) -1) != 28
      || __builtin_clrsbg ((_BitInt(156)) ~(unsigned _BitInt(156)) (unsigned _BitInt(156 - 68)) -1) != 67
      || __builtin_clrsbg ((_BitInt(156)) ~(unsigned _BitInt(156)) (unsigned _BitInt(156 - 92)) -1) != 91
      || __builtin_clrsbg ((_BitInt(156)) ~(unsigned _BitInt(156)) (unsigned _BitInt(156 - 93)) -1) != 92
      || __builtin_ffsg ((_BitInt(156)) (((unsigned _BitInt(156)) -1) << 42)) != 43
      || __builtin_ffsg ((_BitInt(156)) (((unsigned _BitInt(156)) -1) << 57)) != 58
      || __builtin_ffsg ((_BitInt(156)) 0x4000000000000000000000uwb) != 87
      || __builtin_ffsg ((_BitInt(156)) (((unsigned _BitInt(156)) -1) << 149)) != 150
      || __builtin_ffsg ((_BitInt(156)) 2) != 2)
    __builtin_abort ();
  if (parity156 (23008250258685373142923325827291949461178444434uwb) != __builtin_parityg (23008250258685373142923325827291949461178444434uwb)
      || parity156 (41771568792516301628132437740665810252917251244uwb) != __builtin_parityg (41771568792516301628132437740665810252917251244uwb)
      || parity156 (5107402473866766219120283991834936835726115452uwb) != __builtin_parityg (5107402473866766219120283991834936835726115452uwb)
      || popcount156 (50353291748276374580944955711958129678996395562uwb) != __builtin_popcountg (50353291748276374580944955711958129678996395562uwb)
      || popcount156 (29091263616891212550063067166307725491211684496uwb) != __builtin_popcountg (29091263616891212550063067166307725491211684496uwb)
      || popcount156 (64973284306583205619384799873110935608793072026uwb) != __builtin_popcountg (64973284306583205619384799873110935608793072026uwb))
    __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 192
  if (clzd192 (0) != 192
      || clzD192 (0, 42) != 42
      || ctzd192 (0) != 192
      || ctzD192 (0, -1) != -1
      || clrsb192 (0) != 192 - 1
      || ffs192 (0) != 0
      || parity192 (0) != 0
      || popcount192 (0) != 0
      || __builtin_clzg ((unsigned _BitInt(192)) 0, 192 + 32) != 192 + 32
      || __builtin_ctzg ((unsigned _BitInt(192)) 0, 192) != 192
      || __builtin_clrsbg ((_BitInt(192)) 0) != 192 - 1
      || __builtin_ffsg ((_BitInt(192)) 0) != 0
      || __builtin_parityg ((unsigned _BitInt(192)) 0) != 0
      || __builtin_popcountg ((unsigned _BitInt(192)) 0) != 0)
    __builtin_abort ();
  if (clz192 (-1) != 0
      || clzd192 (-1) != 0
      || clzD192 (-1, 15) != 0
      || ctz192 (-1) != 0
      || ctzd192 (-1) != 0
      || ctzD192 (-1, -57) != 0
      || clrsb192 (-1) != 192 - 1
      || ffs192 (-1) != 1
      || parity192 (-1) != 0
      || popcount192 (-1) != 192
      || __builtin_clzg ((unsigned _BitInt(192)) -1) != 0
      || __builtin_clzg ((unsigned _BitInt(192)) -1, 192 + 32) != 0
      || __builtin_ctzg ((unsigned _BitInt(192)) -1) != 0
      || __builtin_ctzg ((unsigned _BitInt(192)) -1, 192) != 0
      || __builtin_clrsbg ((_BitInt(192)) -1) != 192 - 1
      || __builtin_ffsg ((_BitInt(192)) -1) != 1
      || __builtin_parityg ((unsigned _BitInt(192)) -1) != 0
      || __builtin_popcountg ((unsigned _BitInt(192)) -1) != 192)
    __builtin_abort ();
  if (clz192 (((unsigned _BitInt(192)) -1) >> 24) != 24
      || clz192 (((unsigned _BitInt(192)) -1) >> 79) != 79
      || clz192 (1) != 192 - 1
      || clzd192 (((unsigned _BitInt(192)) -1) >> 139) != 139
      || clzd192 (2) != 192 - 2
      || ctz192 (((unsigned _BitInt(192)) -1) << 42) != 42
      || ctz192 (((unsigned _BitInt(192)) -1) << 57) != 57
      || ctz192 (0x4000000000000000000000uwb) != 86
      || ctzd192 (((unsigned _BitInt(192)) -1) << 149) != 149
      || ctzd192 (2) != 1
      || clrsb192 ((unsigned _BitInt(192 - 4)) -1) != 3
      || clrsb192 ((unsigned _BitInt(192 - 28)) -1) != 27
      || clrsb192 ((unsigned _BitInt(192 - 29)) -1) != 28
      || clrsb192 (~(unsigned _BitInt(192)) (unsigned _BitInt(192 - 68)) -1) != 67
      || clrsb192 (~(unsigned _BitInt(192)) (unsigned _BitInt(192 - 92)) -1) != 91
      || clrsb192 (~(unsigned _BitInt(192)) (unsigned _BitInt(192 - 93)) -1) != 92
      || ffs192 (((unsigned _BitInt(192)) -1) << 42) != 43
      || ffs192 (((unsigned _BitInt(192)) -1) << 57) != 58
      || ffs192 (0x4000000000000000000000uwb) != 87
      || ffs192 (((unsigned _BitInt(192)) -1) << 149) != 150
      || ffs192 (2) != 2
      || __builtin_clzg (((unsigned _BitInt(192)) -1) >> 24) != 24
      || __builtin_clzg (((unsigned _BitInt(192)) -1) >> 79) != 79
      || __builtin_clzg ((unsigned _BitInt(192)) 1) != 192 - 1
      || __builtin_clzg (((unsigned _BitInt(192)) -1) >> 139, 192) != 139
      || __builtin_clzg ((unsigned _BitInt(192)) 2, 192) != 192 - 2
      || __builtin_ctzg (((unsigned _BitInt(192)) -1) << 42) != 42
      || __builtin_ctzg (((unsigned _BitInt(192)) -1) << 57) != 57
      || __builtin_ctzg ((unsigned _BitInt(192)) 0x4000000000000000000000uwb) != 86
      || __builtin_ctzg (((unsigned _BitInt(192)) -1) << 149, 192) != 149
      || __builtin_ctzg ((unsigned _BitInt(192)) 2, 192) != 1
      || __builtin_clrsbg ((_BitInt(192)) (unsigned _BitInt(192 - 4)) -1) != 3
      || __builtin_clrsbg ((_BitInt(192)) (unsigned _BitInt(192 - 28)) -1) != 27
      || __builtin_clrsbg ((_BitInt(192)) (unsigned _BitInt(192 - 29)) -1) != 28
      || __builtin_clrsbg ((_BitInt(192)) ~(unsigned _BitInt(192)) (unsigned _BitInt(192 - 68)) -1) != 67
      || __builtin_clrsbg ((_BitInt(192)) ~(unsigned _BitInt(192)) (unsigned _BitInt(192 - 92)) -1) != 91
      || __builtin_clrsbg ((_BitInt(192)) ~(unsigned _BitInt(192)) (unsigned _BitInt(192 - 93)) -1) != 92
      || __builtin_ffsg ((_BitInt(192)) (((unsigned _BitInt(192)) -1) << 42)) != 43
      || __builtin_ffsg ((_BitInt(192)) (((unsigned _BitInt(192)) -1) << 57)) != 58
      || __builtin_ffsg ((_BitInt(192)) 0x4000000000000000000000uwb) != 87
      || __builtin_ffsg ((_BitInt(192)) (((unsigned _BitInt(192)) -1) << 149)) != 150
      || __builtin_ffsg ((_BitInt(192)) 2) != 2)
    __builtin_abort ();
  if (parity192 (4692147078159863499615754634965484598760535154638668598762uwb) != __builtin_parityg (4692147078159863499615754634965484598760535154638668598762uwb)
      || parity192 (1669461228546917627909935444501097256112222796898845183538uwb) != __builtin_parityg (1669461228546917627909935444501097256112222796898845183538uwb)
      || parity192 (5107402473866766219120283991834936835726115452uwb) != __builtin_parityg (5107402473866766219120283991834936835726115452uwb)
      || popcount192 (4033871057575185619108386380181511734118888391160164588976uwb) != __builtin_popcountg (4033871057575185619108386380181511734118888391160164588976uwb)
      || popcount192 (58124766715713711628758119849579188845074973856704521119uwb) != __builtin_popcountg (58124766715713711628758119849579188845074973856704521119uwb)
      || popcount192 (289948065236269174335700831610076764076947650072787325852uwb) != __builtin_popcountg (289948065236269174335700831610076764076947650072787325852uwb))
    __builtin_abort ();
#endif
}
