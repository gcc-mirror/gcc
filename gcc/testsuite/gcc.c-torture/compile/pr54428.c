/* PR c/54428 */
/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

typedef double _Complex C;

C
foo (C x, C y, double z, C w)
{
  return y - z * __builtin_cpow (x, 75) * w;
}
