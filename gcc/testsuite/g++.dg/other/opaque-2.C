/* { dg-do compile { target powerpc-*-eabi* } } */
/* { dg-options "-mcpu=8540 -mabi=spe" } */

#define __vector __attribute__((vector_size(8)))
typedef float __vector __ev64_fs__;

__ev64_fs__ f;
__ev64_opaque__ o;

extern void bar (__ev64_opaque__);

int main ()
{
  f = o;
  o = f;
  bar (f);
  return 0;
}
