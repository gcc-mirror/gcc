/* { dg-do compile } */
/* { dg-options "-O3" } */

#pragma GCC target "+nosve"

int 
t6(int len, void * dummy, unsigned short * __restrict x)
{
  len = len & ~31;
  unsigned int result = 0;
  __asm volatile ("");
  for (int i = 0; i < len; i++)
    result += x[i];
  return result;
}

/* { dg-final { scan-assembler {\tuadalp\tv[0-9]+\.4s, v[0-9]+\.8h} } } */

