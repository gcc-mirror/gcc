/* Test for multiple declarations of an identifier at same block
   scope: only valid case is all extern.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

void
fa0 (void)
{
  int a0; /* { dg-error "previous declaration" } */
  int a0; /* { dg-error "redeclaration" } */
}

void
fa1 (void)
{
  int a1; /* { dg-error "previous declaration" } */
  static int a1; /* { dg-error "redeclaration" } */
}

void
fa2 (void)
{
  int a2; /* { dg-error "previous declaration" } */
  extern int a2; /* { dg-error "follows declaration with no linkage" } */
}

void
fa3 (void)
{
  static int a3; /* { dg-error "previous declaration" } */
  int a3; /* { dg-error "redeclaration" } */
}

void
fa4 (void)
{
  static int a4; /* { dg-error "previous declaration" } */
  static int a4; /* { dg-error "redeclaration" } */
}

void
fa5 (void)
{
  static int a5; /* { dg-error "previous declaration" } */
  extern int a5; /* { dg-error "follows declaration with no linkage" } */
}

void
fa6 (void)
{
  extern int a6; /* { dg-error "previous declaration" } */
  int a6; /* { dg-error "follows extern declaration" } */
}

void
fa7 (void)
{
  extern int a7; /* { dg-error "previous declaration" } */
  static int a7; /* { dg-error "follows extern declaration" } */
}

void
fa8 (void)
{
  extern int a8;
  extern int a8;
}
