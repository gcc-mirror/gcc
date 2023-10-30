/* PR target/105554 */
/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O2 -Wno-psabi -mno-sse3" } */

typedef long long v4di __attribute__((__vector_size__(32)));

__attribute__((target_clones ("arch=core-avx2", "default"))) void
foo (v4di x)
{
}
