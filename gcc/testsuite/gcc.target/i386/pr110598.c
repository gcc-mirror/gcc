/* { dg-do run } */
/* { dg-options "-O2" } */

typedef unsigned long long u64;

#define MAX_SUBTARGET_WORDS 4

int notequal(const void *a, const void *b)
{
  return __builtin_memcmp(a,b,MAX_SUBTARGET_WORDS*sizeof(u64)) != 0;
}
typedef struct FeatureBitset {
  u64 Bits[MAX_SUBTARGET_WORDS];
}FeatureBitset;

__attribute__((noipa))
_Bool is_eq_buggy (const FeatureBitset * lf, const FeatureBitset * rf) {
  u64 Bits_l[MAX_SUBTARGET_WORDS];
  Bits_l[0] = lf->Bits[0]&1;
  Bits_l[1] = 0;
  Bits_l[2] = 0;
  Bits_l[3] = 0;
  u64 Bits_r[MAX_SUBTARGET_WORDS];
  Bits_r[0] = rf->Bits[0]&1;
  Bits_r[1] = 0;
  Bits_r[2] = 0;
  Bits_r[3] = 0;
  return !notequal(Bits_l, Bits_r);
}

__attribute__((noipa))
void bug(void) {
  FeatureBitset lf, rf;
  lf.Bits[0] = rf.Bits[0] = 1;
  lf.Bits[1] = rf.Bits[1] = 1;
  lf.Bits[2] = rf.Bits[2] = 1;
  lf.Bits[3] = rf.Bits[3] = 1;

  _Bool r = is_eq_buggy (&lf, &rf);
  if (!r) __builtin_trap();
}

__attribute__((noipa))
int main(void) {
  bug();
}
