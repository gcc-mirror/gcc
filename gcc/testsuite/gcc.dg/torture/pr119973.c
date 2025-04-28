/* { dg-do run } */
/* { dg-additional-options "-fipa-pta" } */

static int
is_valid_domain_name (const char *string)
{
  const char *s;

  for (s=string; *s; s++)
    if (*s == '.')
      {
        if (string == s)
          return 0;
      }

  return !!*string;
}

int
main (void)
{
  static struct
  {
    const char *name;
    int valid;
  } testtbl[] =
    {
      { ".", 0 },
      { nullptr, 0 }
    };
  int idx;

  for (idx=0; testtbl[idx].name; idx++)
    {
      if (is_valid_domain_name (testtbl[idx].name) != testtbl[idx].valid)
        __builtin_abort ();
    }
  return 0;
}
