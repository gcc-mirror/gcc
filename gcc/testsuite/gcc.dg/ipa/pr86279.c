/* PR ipa/86279 */
/* { dg-do compile } */
/* { dg-options "-fipa-pure-const" } */

typedef __SIZE_TYPE__ size_t;
extern inline __attribute__ ((__always_inline__))
void *
memset (void *x, int y, size_t z)
{
  return __builtin___memset_chk (x, y, z, __builtin_object_size (x, 0));
}

void
foo (unsigned char *x, unsigned char *y, unsigned char *z,
     unsigned char *w, unsigned int v, int u, int t)
{
  int i;
  for (i = 0; i < t; i++)
    {
      memset (z, x[0], v);
      memset (w, y[0], v);
      x += u;
    }
  __builtin_memcpy (z, x, u);
}
