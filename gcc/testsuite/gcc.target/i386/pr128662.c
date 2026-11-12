/* PR target/128662 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-lzcnt -masm=att -mtune-ctrl=avoid_false_dep_for_bsr" } */

#define BITS (__SIZEOF_LONG__ * __CHAR_BIT__)

unsigned long
f1 (unsigned long x)
{
  return BITS - 1 - __builtin_clzl (x);
}

unsigned long
f2 (unsigned long x)
{
  return __builtin_clzl (x) ^ (BITS - 1);
}

/* Zero-extending 32-bit variant (bsr_zext_1 on 64-bit targets).  */
unsigned long
f3 (unsigned int x)
{
  return 31 - __builtin_clz (x);
}

/* { dg-final { scan-assembler-times {xorl\t(%\w+), \1\n\tbsr[lq]\t} 3 } } */
