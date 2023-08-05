/* { dg-do run } */
/* { dg-options "-O2" } */
typedef unsigned long int mp_limb_t;
typedef const mp_limb_t * mp_srcptr;

__attribute__((noipa))
int
refmpn_tstbit_bad (mp_srcptr ptr, unsigned long bit)
{
  return (((ptr)[(bit)/(32 - 0)] & (((mp_limb_t) 1L) << ((bit)%(32 - 0)))) != 0);
}

__attribute__((noipa, optimize(0)))
int
refmpn_tstbit_good (mp_srcptr ptr, unsigned long bit)
{
  return (((ptr)[(bit)/(32 - 0)] & (((mp_limb_t) 1L) << ((bit)%(32 - 0)))) != 0);
}

__attribute__((noipa))
int
refmpn_tstbit (mp_srcptr ptr, unsigned long bit)
{
  if (refmpn_tstbit_bad (ptr, bit) != refmpn_tstbit_good (ptr, bit)) {
      __builtin_trap();
  }
  return refmpn_tstbit_bad (ptr, bit);
}

int main(){
    unsigned long num[] = { 0x3801ff9f, 0x0, 0x0, 0x0 };
    refmpn_tstbit(num, 0);
}
