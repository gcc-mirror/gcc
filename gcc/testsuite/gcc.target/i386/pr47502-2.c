/* { dg-do compile } */
/* { dg-options "-O2 -fno-pic" } */

int
foo (int how, const void *set, void *oset)
{
  int resultvar;
  asm volatile (""
                : "=a" (resultvar)
                : "0" (14) , "b" (how), "c" ((set)), "d" ((oset)), "S" (65 / 8) : "memory", "cc");
  return resultvar;
}
