// PR c++/98688
// { dg-module-do compile { target powerpc*-*-* } }
// { dg-additional-options "-fmodules-ts -mcpu=power10 -mmma" }

import mma_foo0;

typedef unsigned char  vec_t __attribute__((vector_size(16)));

void bar(__vector_quad *dst, vec_t *vec, __vector_pair *pvecp)
{
    foo0 (dst, vec, pvecp);
}
