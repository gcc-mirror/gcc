/* { dg-do compile } */
/* { dg-skip-if "" { ! *-linux-* } } */
/* { dg-options "-w -O2" } */

__thread long long ti;

void f (void)
{
  ti++;
}

void g (long long x)
{
  ti = x;
}
