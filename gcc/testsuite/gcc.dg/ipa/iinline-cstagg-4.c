/* { dg-do compile } */
/* { dg-options "-Os -fdump-ipa-inline-details -fno-early-inlining -fno-ipa-sra -fno-ipa-cp" } */

void long_function (void);

struct Outer
{
  int pad;
  char str[4];
};

static const struct Outer empty_outer = { 1, "" };

int
ret_a (const char *a)
{
  if (a[0])
    {
      long_function ();
      long_function ();
      long_function ();
      long_function ();
      long_function ();
      long_function ();
      long_function ();
      long_function ();
    }
  return a[0];
}

static int
passthru (const struct Outer *p)
{
  long_function ();
  return ret_a (p->str);
}

int
test_os (void)
{
  return passthru (&empty_outer);
}

/* { dg-final { scan-ipa-dump "ret_a\[^\\n\]*inline copy in test_os" "inline" } } */
