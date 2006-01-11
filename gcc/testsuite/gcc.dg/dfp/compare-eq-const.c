/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

/* C99 6.5.9 Equality operators.
   Compare decimal float constants against each other. */

extern void link_error (void);

int
main ()
{
  /* Compare like-typed positive constants. */
  if (2.0df != 2.0df)
    link_error ();

  /* Compare decimal float constants of different types. */
  if (500e-2dl != 0.05e2df)
    link_error ();

  /* Binary floating point introduces errors to decimal values. */
  if (1.4 + 1.4 + 1.4 == 4.2)
    link_error ();

  /* But, this looks more like what one would expect. */
  if (1.4dd + 1.4dd + 1.4dd != 4.2dd)
    link_error ();

  return 0;
}
