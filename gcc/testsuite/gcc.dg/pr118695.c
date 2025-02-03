/* { dg-do compile } */
/* { dg-options "-O" } */

void * g(int obj)
{
  char *t = (char*)&obj;
  t -= 1;
  return *(int**)t;
}
