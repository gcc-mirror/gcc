/* PR target/93141 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-not "cmp\[lq]\t" } } */
/* { dg-final { scan-assembler-not "adc\[lq]\t" } } */
/* { dg-final { scan-assembler-times "seto\t%" 7 } } */
/* { dg-final { scan-assembler-times "sbb\[lq]\t" 5 } } */

#ifdef __x86_64__
typedef unsigned __int128 U;
typedef unsigned long long HU;
typedef signed __int128 S;
#else
typedef unsigned long long U;
typedef signed int HU;
typedef signed long long S;
#endif
int o;

S
qux (S x, S y)
{
  S z;
  o = __builtin_sub_overflow (x, y, &z);
  return z;
}

S
quux (S x)
{
  S z;
  o = __builtin_sub_overflow (x, ((S) 0xdeadbee) << (sizeof (S) * __CHAR_BIT__ / 2), &z);
  return z;
}

S
corge (S x)
{
  S z;
  o = __builtin_sub_overflow (x, (((S) 0xdeadbee) << (sizeof (S) * __CHAR_BIT__ / 2))
				 | (S) 0xbeedead, &z);
  return z;
}

S
grault (S x)
{
  S z;
  o = __builtin_sub_overflow (x, -((S) 0xdeadbee) << (sizeof (S) * __CHAR_BIT__ / 2), &z);
  return z;
}

S
garply (S x)
{
  S z;
  o = __builtin_sub_overflow (x, (-(((S) 0xdeadbee) << (sizeof (S) * __CHAR_BIT__ / 2)))
				 | (S) 0xbeedead, &z);
  return z;
}

S
waldo (S x)
{
  S z;
  o = __builtin_sub_overflow (x, (S) ((((S) 0xdeadbee) << (sizeof (S) * __CHAR_BIT__ / 2))
				      | -(HU) 0xbeedead), &z);
  return z;
}

S
fred (S x)
{
  S z;
  o = __builtin_sub_overflow (x, (S) ((-(((S) 0xdeadbee) << (sizeof (S) * __CHAR_BIT__ / 2)))
				      | -(HU) 0xbeedead), &z);
  return z;
}
