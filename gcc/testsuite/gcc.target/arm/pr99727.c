/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-options "-O3" } */
/* { dg-add-options arm_v8_1m_mve } */

char a[7][25][15][14];
void b() {
  for (int c;; ++c)
    for (int d = 0; d < 25; ++d)
      for (int e = 0; e < 15; ++e)
	for (int f = 0; f < 14; ++f)
	  a[c][d][e][f] = 1;
}
