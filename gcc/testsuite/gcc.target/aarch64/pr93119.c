/* { dg-require-effective-target fpic } */
/* { dg-options "-mtls-dialect=trad -fpic" } */

__thread int g_tlsdata;

int func1()
{
  g_tlsdata++;
  return g_tlsdata;
}
