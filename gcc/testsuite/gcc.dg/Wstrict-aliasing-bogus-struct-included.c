/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing -fstrict-aliasing" } */


struct U
{
  float f;
  int i;
};


int foo ()
{
  struct U u;
  float *pf = (float*)&u;  /* { dg-bogus "float included in struct U" } */
  *pf = 2.0;
  return u.i;
}
