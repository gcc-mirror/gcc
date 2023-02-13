/* { dg-do compile } */
/* { dg-additional-options "-Ofast --param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

/* From TSVC s352.  */

typedef float real_t;

#define LEN_1D 32000
#define LEN_2D 256

real_t a[LEN_1D],b[LEN_1D];
real_t foo ()
{
  real_t dot = (real_t)0.;
  for (int i = 0; i < LEN_1D; i += 5) {
      dot = dot + a[i] * b[i] + a[i + 1] * b[i + 1] + a[i + 2]
	  * b[i + 2] + a[i + 3] * b[i + 3] + a[i + 4] * b[i + 4];
  }

  return dot;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
