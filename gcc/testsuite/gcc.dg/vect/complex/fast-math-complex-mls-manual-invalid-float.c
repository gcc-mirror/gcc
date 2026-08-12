/* { dg-do compile } */
/* { dg-additional-options "-O3 -fcx-limited-range -fno-signed-zeros" } */
/* { dg-require-effective-target vect_complex_add_float } */
/* { dg-add-options arm_v8_3a_complex_neon } */

void
manual_invalid_fms (float *__restrict d, float *__restrict c,
		    float *__restrict a, float *__restrict b)
{
  for (int r = 0; r < 100; r += 2)
    {
      int i = r + 1;
      float cr = c[r];
      float ci = c[i];
      float ar = a[r];
      float ai = a[i];
      float br = b[r];
      float bi = b[i];
      d[r] = cr + ar * br - ai * bi;
      d[i] = ci - (ar * bi + ai * br);
    }
}

/* { dg-final { scan-tree-dump-not "Found COMPLEX_FMS pattern" "vect" } } */
/* { dg-final { scan-tree-dump-not "Found COMPLEX_FMS_CONJ" "vect" } } */
/* { dg-final { scan-tree-dump-not "Found COMPLEX_MUL" "vect" } } */
/* { dg-final { scan-tree-dump-not "add new stmt: \[^\n\r]*COMPLEX_FMS" "vect" } } */
