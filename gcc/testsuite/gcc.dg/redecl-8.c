/* Test for multiple declarations and composite types.  Diagnosis of
   completion incompatible with implicit initializer.  */

/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-g" } */

static int x[];

void
f (void)
{
  extern int x[2]; /* { dg-error "completed incompatibly" } */
}

/* The following is OK.  */

static int y[];
void
g (void)
{
  extern int y[1];
}
