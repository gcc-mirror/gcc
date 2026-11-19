/* PR target/128662 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-bmi -masm=att -mtune=nehalem -mtune-ctrl=avoid_false_dep_for_bsr" } */

unsigned long
f1 (unsigned long x)
{
  return __builtin_ctzl (x);
}

/* Zero-extending variant (*ctzsidi2_<s>ext on 64-bit targets).  */
unsigned long
f2 (unsigned int x)
{
  return __builtin_ctz (x);
}

/* { dg-final { scan-assembler-times {xorl\t(%\w+), \1\n\tbsf[lq]\t} 2 } } */
