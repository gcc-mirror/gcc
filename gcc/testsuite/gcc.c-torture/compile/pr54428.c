/* PR c/54428 */

typedef double _Complex C;

C
foo (C x, C y, double z, C w)
{
  return y - z * __builtin_cpow (x, 75) * w;
}
