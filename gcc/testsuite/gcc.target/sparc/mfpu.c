/* Reported by Peter A. Krauss <peter.a.krauss@web.de> */

/* { dg-do compile } */
/* { dg-options "-mfpu" } */

float square(float x)
{
  return x * x;
}

/* { dg-final { scan-assembler "fmuls" } } */
