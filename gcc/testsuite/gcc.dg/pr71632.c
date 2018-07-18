/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-if-convert" } */

void
foo (double d, double *p, double *q)
{
  int i;
  for (i = 0; i < 64; i++)
    {
      double t1 = d > p[0] ? 1.0 : 0.0;
      double t2 = t1 > p[1] ? 1.0 : 0.0;
      double t3 = t2 > p[2] ? 1.0 : 0.0;
      double t4 = t3 > p[3] ? 1.0 : 0.0;
      double t5 = t4 > p[4] ? 1.0 : 0.0;
      double t6 = t5 > p[5] ? 1.0 : 0.0;
      double t7 = t6 > p[6] ? 1.0 : 0.0;
      double t8 = t7 > p[7] ? 1.0 : 0.0;
      double t9 = t8 > p[8] ? 1.0 : 0.0;
      double t10 = t9 > p[9] ? 1.0 : 0.0;
      double t11 = t10 > p[10] ? 1.0 : 0.0;
      double t12 = t11 > p[11] ? 1.0 : 0.0;
      double t13 = t12 > p[12] ? 1.0 : 0.0;
      double t14 = t13 > p[13] ? 1.0 : 0.0;
      double t15 = t14 > p[14] ? 1.0 : 0.0;
      double t16 = t15 > p[15] ? 1.0 : 0.0;
      double t17 = t16 > p[16] ? 1.0 : 0.0;
      double t18 = t17 > p[17] ? 1.0 : 0.0;
      double t19 = t18 > p[18] ? 1.0 : 0.0;
      double t20 = t19 > p[19] ? 1.0 : 0.0;
      double t21 = t20 > p[20] ? 1.0 : 0.0;
      double t22 = t21 > p[21] ? 1.0 : 0.0;
      double t23 = t22 > p[22] ? 1.0 : 0.0;
      double t24 = t23 > p[23] ? 1.0 : 0.0;
      double t25 = t24 > p[24] ? 1.0 : 0.0;
      double t26 = t25 > p[25] ? 1.0 : 0.0;
      double t27 = t26 > p[26] ? 1.0 : 0.0;
      double t28 = t27 > p[27] ? 1.0 : 0.0;
      double t29 = t28 > p[28] ? 1.0 : 0.0;
      double t30 = t29 > p[29] ? 1.0 : 0.0;
      double t31 = t30 > p[30] ? 1.0 : 0.0;
      double t32 = t31 > p[31] ? 1.0 : 0.0;
      double t33 = t32 > p[32] ? 1.0 : 0.0;
      double t34 = t33 > p[33] ? 1.0 : 0.0;
      double t35 = t34 > p[34] ? 1.0 : 0.0;
      double t36 = t35 > p[35] ? 1.0 : 0.0;
      double t37 = t36 > p[36] ? 1.0 : 0.0;
      double t38 = t37 > p[37] ? 1.0 : 0.0;
      double t39 = t38 > p[38] ? 1.0 : 0.0;
      *q = t39;
      p += 39;
      q++;
    }
}
