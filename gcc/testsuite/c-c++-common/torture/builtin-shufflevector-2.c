/* { dg-do run}  */
/* PR target/101529 */
typedef unsigned char C;
typedef unsigned char __attribute__((__vector_size__ (8))) V;
typedef unsigned char __attribute__((__vector_size__ (32))) U;

C c;

/* aarch64 used to miscompile foo to just return a vector of 0s */
V
foo (V v)
{
  v |= __builtin_shufflevector (c * v, (U) (0 == (U){ }),
				0, 1, 8, 32, 8, 20, 36, 36);
  return v;
}

int
main (void)
{
  V v = foo ((V) { });
  for (unsigned i = 0; i < sizeof (v); i++)
    if (v[i] != (i >= 2 ? 0xff : 0))
      __builtin_abort ();
  return 0;
}

// On i?86-*-* an ABI warning would actually surface.
// { dg-prune-output "MMX vector (argument|return) without MMX enabled changes the ABI" }
