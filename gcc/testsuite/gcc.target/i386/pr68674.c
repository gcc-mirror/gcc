/* PR target/68674 */
/* { dg-do compile } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2" } */

typedef int v8si __attribute__((vector_size(32)));

v8si a;

 __attribute__((target("avx")))
v8si
foo()
{
    return a;
}
