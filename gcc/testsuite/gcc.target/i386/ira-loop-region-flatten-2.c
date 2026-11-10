/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mabi=sysv -mapxf -fdump-rtl-ira" } */

typedef float v4sf __attribute__ ((vector_size (16)));
extern void use (float);

v4sf
vec_cross (v4sf *a, int n)
{
  v4sf f0 = a[0], f1 = a[1], f2 = a[2], f3 = a[3];
  v4sf f4 = a[4], f5 = a[5], f6 = a[6], f7 = a[7];
  for (int k = 0; k < n; k++)
    {
      use (a[k][0]);
      f0 += f1;
      f3 += f4;
      f6 += f7;
      use (a[k][1]);
      f1 += f2;
      f4 += f5;
      f7 += f0;
      use (a[k][2]);
      f2 += f3;
      f5 += f6;
    }
  return f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7;
}

/* { dg-final { scan-rtl-dump-not {flatten-cost:[^\n]*keep} "ira" } } */
