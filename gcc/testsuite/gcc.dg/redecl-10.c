/* Test for multiple declarations and composite types.  Check we don't
   ICE with nested initializers.  */

/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-g" } */

static int w[];
void
f (void)
{
  extern int w[] = { 1, 2 }; /* { dg-error "has both" } */
}

int x[];
void
g (void)
{
  extern int x[] = { 3, 4, 5 }; /* { dg-error "has both" } */
}

static int y[];
void
h (void)
{
  extern int y[] = { 6 }; /* { dg-error "has both" } */
}

int z[];
void
i (void)
{
  extern int z[] = { 7 }; /* { dg-error "has both" } */
}
