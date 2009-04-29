/* PR39565: Static variable leaves undefined symbol in object file.
   Variables codestrs_4 and codestrs_8 should not be undefined,
   when this program is compiled at -O2. As a basic sanity test,
   verify that this program compiles into an executable and runs. */

/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);
extern void exit (int);
extern int strcmp (const char *, const char *);

static const union codestrs_t_4 {
  struct {
    char str_2[sizeof ("Illegal opcode")];
  };
  char str[0];
} codestrs_4 = { {"Illegal opcode"} };

static const union codestrs_t_8 {
  struct {
    char str_12[sizeof ("Integer divide by zero")];
  };
  char str[0];
} codestrs_8 = { {"Integer divide by zero"} };

const char *
psiginfo (int pinfo)
{
  const char *base = ((void *)0);

  switch (pinfo)
    {
    case 4: base = codestrs_4.str; break;
    case 8: base = codestrs_8.str; break;
    }
  return base;
}

int
main (void)
{
  if (strcmp (psiginfo (4), codestrs_4.str))
    abort ();
  if (strcmp (psiginfo (8), codestrs_8.str))
    abort ();
  exit (0);
}
