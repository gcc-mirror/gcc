/* { dg-do compile } */
/* { dg-additional-options "-O3 -fcx-limited-range -fno-signed-zeros -fdump-tree-vect-details" } */
/* { dg-require-effective-target vect_complex_add_double } */
/* { dg-add-options arm_v8_3a_complex_neon } */

void
fms_out (_Complex double *__restrict d, _Complex double *__restrict c,
	 _Complex double *__restrict a, _Complex double *__restrict b,
	 int n)
{
  for (int i = 0; i < n; ++i)
    d[i] = c[i] - a[i] * b[i];
}

/* { dg-final { scan-tree-dump "add new stmt: \[^\n\r]*COMPLEX_FMS \\(" "vect" } } */
/* { dg-final { scan-tree-dump-not "Found COMPLEX_FMS_CONJ" "vect" } } */
/* { dg-final { scan-tree-dump-not "Found COMPLEX_MUL" "vect" } } */
