/* PR target/pr100660.  */
/* { dg-do compile } */
/* { dg-options "-mavx2 -O" } */

typedef char v16qi __attribute__ ((vector_size (16)));
v16qi
f5 (v16qi a, v16qi b)
{
  __builtin_ia32_pcmpgtb128 (a, b);
}
