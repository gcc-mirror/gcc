/* PR target/105927 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O1 -fno-tree-dce -mtune=k6-3 -msse2" } */

typedef _Float16 __attribute__((__vector_size__(4))) U;
typedef _Float16 __attribute__((__vector_size__(2))) V;
typedef short __attribute__((__vector_size__(4))) W;
V v;
U u;

extern void bar(W i);

void
foo(void)
{
  U x = __builtin_shufflevector(v, u, 2, 0);
  bar(x >= 0);
}
