/* { dg-do compile } */
/* { dg-options "-O1" } */
void foo(int *p, long i, int j)
{
  do {
      p[i]++;
  } while (i += j);
}
