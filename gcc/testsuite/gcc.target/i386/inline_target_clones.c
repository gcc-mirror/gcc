/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O3 -march=x86-64" } */
/* { dg-final { scan-assembler-not "call\[ \t\]+callee" } } */

float callee (float a, float b, float c, float d,
	      float e, float f, float g, float h)
{
  return a * b + c * d + e * f + g + h + a * c + b * c
    + a * d + b * e + a * f + c * h + 
    b * (a - 0.4f) * (c + h) * (b + e * d) - a / f * h;
}

__attribute__((target_clones("default","arch=icelake-server")))
void caller (int n, float *a,
	     float c1, float c2, float c3,
	     float c4, float c5, float c6,
	     float c7)
{
  for (int i = 0; i < n; i++)
    {
      a[i] = callee (a[i], c1, c2, c3, c4, c5, c6, c7);
    }
}
