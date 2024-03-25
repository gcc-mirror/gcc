/* PR target/109977 */
/* { dg-do compile } */
/* { dg-options "-Og" } */

typedef double __attribute__((__vector_size__ (__SIZEOF_DOUBLE__))) V;
typedef double __attribute__((__vector_size__ (__SIZEOF_DOUBLE__*2))) W;
V v;
int i;
extern void bar (void *);

void
foo (void)
{
  W w = __builtin_shufflevector (v, (W) { }, 0, 0);
  bar (&w);
}
