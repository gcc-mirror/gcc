/* { dg-do run } */
/* { dg-options "-O0" } */

extern void abort (void);
extern void exit (int);

/* Check that out-of-bounds case warnings work in the case that the
   testing expression is promoted.  */
int
foo1 (unsigned char i)
{
  switch (i)
    {
    case -1:   /* { dg-warning "case label value is less than minimum value for type" } */
      return 1;
    case 256:  /* { dg-warning "case label value exceeds maximum value for type" } */
      return 2;
    default:
      return 3;
    }
}

/* Like above, but for case ranges that need to be satured.  */
int
foo2 (unsigned char i)
{
  switch (i)
    {
    case -1 ... 1:   /* { dg-warning "lower value in case label range less than minimum value for type" } */
      return 1;
    case 254 ... 256:  /* { dg-warning "upper value in case label range exceeds maximum value for type" } */
      return 2;
    default:
      return 3;
    }
}

int
main (void)
{
  if (foo1 (10) != 3)
    abort ();
  if (foo2 (10) != 3)
    abort ();
  exit (0);
}

