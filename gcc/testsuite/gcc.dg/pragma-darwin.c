/* Darwin (Mac OS X) pragma exercises.  */

/* { dg-do run { target powerpc-*-darwin* } } */
/* { dg-options "-O -Wunused" } */

/* The mark pragma is to help decorate IDEs.  */

extern void abort(void);

#pragma mark hey hey ho

/* The options pragma used to do a lot, now it's only for emulating
   m68k alignment rules in structs.  */

#pragma options 23  /* { dg-error "malformed '#pragma options'" } */
#pragma options align  /* { dg-error "malformed '#pragma options'" } */
#pragma options align mac68k /* { dg-error "malformed '#pragma options'" } */
#pragma options align=45 /* { dg-error "malformed '#pragma options'" } */
#pragma options align=foo /* { dg-error "malformed '#pragma options align" } */

#pragma options align=mac68k
struct s1 { short f1; int f2; };
#pragma options align=power
struct s2 { short f1; int f2; };
#pragma options align=mac68k
struct s3 { short f1; int f2; };
#pragma options align=reset
struct s4 { short f1; int f2; };

#pragma options align=mac68k foo /* { dg-warning "junk at end of '#pragma options'" } */

/* Segment pragmas don't do anything anymore.  */

#pragma segment foo

int
main ()
{
  int x, z;  /* { dg-warning "unused variable" } */
  #pragma unused (x, y)

  if (sizeof (struct s1) != 6)
    abort ();
  if (sizeof (struct s2) != 8)
    abort ();
  if (sizeof (struct s3) != 6)
    abort ();
  if (sizeof (struct s4) != 8)
    abort ();
  return 0;
}

void
unused_err_test ()
{
  int a, b;
  /* Trying to match on '(' or ')' gives regexp headaches, use . instead.  */
#pragma unused  /* { dg-error "missing '.' after '#pragma unused" } */
#pragma unused (a  /* { dg-error "missing '.' after '#pragma unused" } */
#pragma unused (b) foo /* { dg-warning "junk at end of '#pragma unused'" } */
}
