/* { dg-require-effective-target fpic } */
/* { dg-options "-mtls-dialect=trad -fpic" } */

__thread int abc;
void
foo ()
{
  int *p;
  p = &abc;
}
