/* PR target/104104 */
/* { dg-do assemble { target vect_simd_clones } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-march=alderlake -masm=intel -O1 -fallow-store-data-races -funroll-all-loops" } */

__attribute__ ((simd)) short int
foo (void)
{
  return 0;
}
