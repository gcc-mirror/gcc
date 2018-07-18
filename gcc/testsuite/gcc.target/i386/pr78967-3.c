/* PR target/78967 */
/* { dg-do assemble { target { ! ia32 } } } */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;

struct S1
{
  unsigned char pad1;
  unsigned char val;
  unsigned short pad2;
};

extern unsigned char t[256];

struct S1 foo (struct S1 a, size_t i)
{
  register size_t _i __asm ("r10") = i;

  asm volatile ("" : "+r" (_i));
  a.val = t[_i];

  return a;
}
