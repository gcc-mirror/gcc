/* PR middle-end/108237 */

typedef unsigned char __attribute__((__vector_size__ (1))) U;
typedef unsigned long long __attribute__((__vector_size__ (16))) V;

U u;
V v;

V
foo (void)
{
  V w = v != ((unsigned char) ((unsigned char) u == u) & v);
  return w;
}
