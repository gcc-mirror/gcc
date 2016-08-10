/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_condition } */

float a,b,c;

float z[1024]; int ok[1024];

void foo()
{
  int i;

  for (i=0; i!=1024; ++i)
    {
      float rR = a*z[i];
      float rL = b*z[i];
      float rMin = (rR!=rL) ? rR : 0.0;
      ok[i] = rMin-c;
    }
}

/* { dg-final { scan-assembler-not "\[ \t\]not\[ \t\]" } } */
