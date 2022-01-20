/* { dg-compile } */

int
foo (char* p)
{
  return p + 1000 < p;
}
