/* Test for multiple declarations and composite types.  */

/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

int y[];
void
g (void)
{
  extern int y[1];
}
