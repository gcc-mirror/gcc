enum { TYPE_A };

/* Verify that the incorrect "TYPE_B" etc don't get re-used for
   suggestions for the later incorrect values.  */

void pr83056(void)
{
  int b = TYPE_B; /* { dg-error "did you mean 'TYPE_A'" } */
  int c = TYPE_C; /* { dg-error "did you mean 'TYPE_A'" } */
  int d = TYPE_D; /* { dg-error "did you mean 'TYPE_A'" } */
}
