/* { dg-do run } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-add-options tls } */

extern void abort (void);

__thread int tls_gd __attribute__((tls_model("global-dynamic"))) = 0;

int get_gd (void)
{
  return tls_gd;
}

int *get_gdp (void)
{
  return &tls_gd;
}

int main (void)
{
  int val;

  val = get_gd ();
  if (val != 0)
    abort ();

  val = *get_gdp ();
  if (val != 0)
    abort ();

  return 0;
}
