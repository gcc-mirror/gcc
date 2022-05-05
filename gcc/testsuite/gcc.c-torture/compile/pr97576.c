/* { dg-require-effective-target non_strict_prototype } */

void
pc (void);

void __attribute__ ((simd))
ty (void);

void __attribute__ ((simd))
gf ()
{
  ty ();
}

void __attribute__ ((simd))
ty (void)
{
  gf (pc);
  gf (gf);
}
