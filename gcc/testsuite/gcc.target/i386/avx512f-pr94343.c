/* PR target/94343 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mno-avx512vl" } */
/* { dg-final { scan-assembler-not "vpternlogd\[^\n\r]*xmm\[0-9]*" } } */

typedef int __v4si __attribute__((vector_size (16)));

__v4si
foo (__v4si a)
{
  return ~a;
}
