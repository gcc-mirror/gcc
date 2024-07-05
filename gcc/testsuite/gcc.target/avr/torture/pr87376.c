/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options "-std=gnu99" } */

typedef __UINT64_TYPE__ uint64_t;

extern const __memx uint64_t aa __asm ("real_aa");
extern const uint64_t bb __asm ("real_bb");

const __memx uint64_t real_aa = 0x1122334455667788;
const uint64_t real_bb        = 0x0908070605040302;

__attribute__((noinline,noclone))
uint64_t add1 (void)
{
  return aa + bb;
}

__attribute__((noinline,noclone))
uint64_t add2 (void)
{
  return bb + aa;
}

__attribute__((noinline,noclone))
uint64_t sub1 (void)
{
  return aa - bb;
}

__attribute__((noinline,noclone))
uint64_t sub2 (void)
{
  return bb - aa;
}

__attribute__((noinline,noclone))
uint64_t neg1 (void)
{
  return -aa;
}

int main (void)
{
  if (neg1() != -real_aa)
  __builtin_exit (__LINE__);

  if (add1() != real_aa + real_bb)
    __builtin_exit (__LINE__);

  if (add2() != real_bb + real_aa)
    __builtin_exit (__LINE__);

  if (sub1() != real_aa - real_bb)
    __builtin_exit (__LINE__);
  
  if (sub2() != real_bb - real_aa)
    __builtin_exit (__LINE__);

  return 0;
}
