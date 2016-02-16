/* { dg-do compile } */
/* { dg-options "-fstack-limit-register=r2" } */
void foo ()
{
  int N = 2;
  int slots[N];

}
