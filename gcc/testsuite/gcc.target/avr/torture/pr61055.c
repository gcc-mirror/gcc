/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-options { -fno-peephole2 } } */

#include <stdlib.h>

typedef __UINT16_TYPE__ uint16_t;
typedef __INT16_TYPE__  int16_t;
typedef __UINT8_TYPE__  uint8_t;

uint8_t __attribute__((noinline,noclone))
fun_inc (uint8_t c0)
{
  register uint8_t c asm ("r15") = c0;

  /* Force target value into R15 (lower register)  */
  asm ("" : "+l" (c));

  c++;
  if (c >= 0x80)
    c = 0;
  
  asm ("" : "+l" (c));

  return c;
}

uint8_t __attribute__((noinline,noclone))
fun_dec (uint8_t c0)
{
  register uint8_t c asm ("r15") = c0;

  /* Force target value into R15 (lower register)  */
  asm ("" : "+l" (c));

  c--;
  if (c < 0x80)
    c = 0;
  
  asm ("" : "+l" (c));

  return c;
}


uint8_t __attribute__((noinline,noclone))
fun_neg (uint8_t c0)
{
  register uint8_t c asm ("r15") = c0;

  c = -c;
  if (c >= 0x80)
    c = 0;

  return c;
}

uint16_t __attribute__((noinline,noclone))
fun_adiw (uint16_t c0)
{
  register uint16_t c asm ("r24") = c0;

  /* Force target value into R24 (for ADIW) */
  asm ("" : "+r" (c));

  c += 2;
  if (c >= 0x8000)
    c = 0;

  asm ("" : "+r" (c));
  
  return c;
}


int main()
{
  if (fun_inc (0x7f) != 0)
    abort();
  
  if (fun_neg (0x80) != 0)
    abort();
  
  if (fun_adiw (0x7ffe) != 0)
    abort();

  exit (0);
  return 0;
}
