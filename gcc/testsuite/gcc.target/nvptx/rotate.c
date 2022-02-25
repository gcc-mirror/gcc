/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps -misa=sm_35" } */

#define MASK 0x1f

unsigned int
rotl (unsigned int val, unsigned int cnt) {
  cnt &= MASK;
  return (val << cnt) | (val >> (-cnt & MASK));
}

unsigned int
rotr (unsigned int val, unsigned int cnt) {
  cnt &= MASK;
  return (val >> cnt) | (val << (-cnt & MASK));
}

/* { dg-final { scan-assembler-times "shf.l.wrap.b32" 1 } } */
/* { dg-final { scan-assembler-times "shf.r.wrap.b32" 1 } } */
/* { dg-final { scan-assembler-not "and.b32" } } */
