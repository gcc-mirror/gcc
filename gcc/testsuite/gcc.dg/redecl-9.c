/* Test for multiple declarations and composite types, as in bug
   13801.  Test types saved from outer scopes are up to date.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

int x[];

void
f (void)
{
  extern int x[];
}

int x[10];

void
g (void)
{
  int x;
  {
    extern int x[10];
  }
}

void
h (void)
{
  sizeof (x);
}
