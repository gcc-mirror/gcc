/* { dg-do compile } */
/* { dg-options "-O2" } */
extern int a; /* { dg-error "declared weak after being used" } */
t()
{
  return &a!=0;
}
extern int a __attribute__ ((weak));
