/* { dg-do run } */
/* { dg-additional-options "-std=gnu99" } */

typedef __UINT64_TYPE__ uint64_t;

extern const uint64_t aa __asm ("real_aa");
extern const uint64_t bb __asm ("real_bb");

__attribute__((used)) const uint64_t real_aa = 0x1122334455667788;
__attribute__((used)) const uint64_t real_bb = 0x0908070605040302;

__attribute__((noipa))
uint64_t add1 (const uint64_t *aa, const uint64_t *bb)
{
  return *aa + *bb;
}

#ifdef __FLASH
extern const __flash uint64_t fa __asm ("real_fa");
extern const __flash uint64_t fb __asm ("real_fb");

__attribute__((used)) const __flash uint64_t real_fa = 0x1122334455667788;
__attribute__((used)) const __flash uint64_t real_fb = 0x0908070605040302;

__attribute__((noipa))
uint64_t add2 (const __flash uint64_t *aa, const uint64_t *bb)
{
  return *aa + *bb;
}

uint64_t add3 (const uint64_t *aa, const __flash uint64_t *bb)
{
  return *aa + *bb;
}

uint64_t add4 (const __flash uint64_t *aa, const __flash uint64_t *bb)
{
  return *aa + *bb;
}
#endif /* have __flash */

int main (void)
{
  if (add1 (&aa, &bb) != real_aa + real_bb)
    __builtin_exit (__LINE__);

#ifdef __FLASH
  if (add2 (&fa, &bb) != real_fa + real_bb)
    __builtin_exit (__LINE__);

  if (add3 (&aa, &fb) != real_aa + real_fb)
    __builtin_exit (__LINE__);

  if (add4 (&fa, &fb) != real_fa + real_fb)
    __builtin_exit (__LINE__);
#endif

  return 0;
}
