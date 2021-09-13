/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -msse2" } */

typedef long v2di __attribute__((vector_size(16)));
typedef int v4si __attribute__((vector_size(16)));

void foo (v4si *p, v2di *q)
{
  union { v2di a; v4si b; } u;
  u.a = *q;
  (*p)[0] = u.b[0];
  (*p)[1] = u.b[2];
  (*p)[2] = u.b[1];
  (*p)[3] = u.b[3];
}

void bar (v4si *p, __int128_t *q)
{
  union { __int128_t a; v4si b; } u;
  u.a = *q;
  (*p)[0] = u.b[1];
  (*p)[1] = u.b[2];
  (*p)[2] = u.b[1];
  (*p)[3] = u.b[3];
}

/* Both functions should end up with sth like
     [v]pshufd $val, (%esi), %xmm0
     [v]movdqa %xmm0, (%edi)
     ret
   recognized by SLP vectorization involving an existing "vector".  */
/* { dg-final { scan-assembler-not "punpck" } } */
/* { dg-final { scan-assembler-times "pshufd" 2 } } */
