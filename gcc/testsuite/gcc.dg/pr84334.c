/* PR tree-optimization/84334 */
/* { dg-do compile } */
/* { dg-options "-Ofast -frounding-math" } */

float
foo (void)
{
  float a = 9.999999974752427078783512115478515625e-7f;
  float b = 1.999999994950485415756702423095703125e-6f;
  float c = 4.999999873689375817775726318359375e-6f;
  return a + b + c;
}
