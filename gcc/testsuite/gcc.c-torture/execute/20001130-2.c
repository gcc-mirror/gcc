static int which_alternative = 3;

static const char *i960_output_ldconst (void);

static const char *
output_25 (void)
{
  switch (which_alternative)
    {
    case 0:
      return "mov	%1,%0";
    case 1:
      return i960_output_ldconst ();
    case 2:
      return "ld	%1,%0";
    case 3:
      return "st	%1,%0";      
    }
}

static const char *i960_output_ldconst (void)
{
  return "foo";
}
int main(void)
{
  const char *s = output_25 () ;
  if (s[0] != 's')
    abort ();
  exit (0);
}
