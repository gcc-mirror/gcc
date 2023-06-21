/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-loop-vectorize" } */

extern int a[][4], b[][4], c[][4], d[4], e[4];
void f()
{
  int t0 = a[0][3];
  int t1 = a[1][3];
  int t2 = a[2][3];
  int t3 = a[3][3];
  int a0 = 0, a1 = 0, a2 = 0, a3 = 0, b0 = 0, b1 = 0, b2 = 0, b3 = 0;
  for (int j = 0; j < 100; ++j)
    for (int i = 0; i < 400; i += 4)
      {
	a0 += b[i][3] * t0;
	a1 += b[i][2] * t1;
	a2 += b[i][1] * t2;
	a3 += b[i][0] * t3;
	b0 += c[i][3] * t0;
	b1 += c[i][2] * t1;
	b2 += c[i][1] * t2;
	b3 += c[i][0] * t3;
      }
  d[0] = a0;
  d[1] = a1;
  d[2] = a2;
  d[3] = a3;
  e[0] = b0;
  e[1] = b1;
  e[2] = b2;
  e[3] = b3;
}

/* On older powerpc hardware (POWER7 and earlier), the default flag
   -mno-allow-movmisalign prevents vectorization.  On POWER8 and later,
   when vect_hw_misalign is true, vectorization occurs.  For other
   targets, ! vect_no_align is a sufficient test.  */

/* { dg-final { scan-tree-dump-times "add new stmt: \[^\\n\\r\]* = VEC_PERM_EXPR" 3 "slp1" { target { { vect_int_mult && vect_perm } && { { ! powerpc*-*-* } || { vect_hw_misalign } } } } } } */
