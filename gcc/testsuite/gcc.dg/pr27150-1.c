/* { dg-do compile } */
/* { dg-options "-O2" } */
int g(int f)
{
  return (&f)!=0;
}

