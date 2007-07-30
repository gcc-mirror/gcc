/* Darwin (Mac OS X) pragma exercises.  */

/* { dg-do run { target *-*-darwin* } } */
/* { dg-options "-O -Wunused" } */

/* The mark pragma is to help decorate IDEs.  */

extern void abort(void);

#pragma mark hey hey ho

/* The options pragma used to do a lot, now it's only for emulating
   m68k alignment rules in structs.  */

#pragma options 23  /* { dg-warning "malformed '#pragma options'" } */
#pragma options align  /* { dg-warning "malformed '#pragma options'" } */
#pragma options align natural /* { dg-warning "malformed '#pragma options'" } */
#pragma options align=45 /* { dg-warning "malformed '#pragma options'" } */
#pragma options align=foo /* { dg-warning "malformed '#pragma options align" } */

#ifndef __LP64__
#pragma options align=mac68k
struct s1 { short f1; int f2; };
#endif
#pragma options align=power
struct s2 { short f1; int f2; };
#ifndef __LP64__
#pragma options align=mac68k
struct s3 { short f1; int f2; };
#endif
#pragma options align=reset
struct s4 { short f1; int f2; };

#pragma options align=natural foo /* { dg-warning "junk at end of '#pragma options'" } */
/* { dg-warning "malformed '#pragma options align={mac68k|power|reset}', ignoring" "ignoring" { target *-*-* } 34 } */

/* Segment pragmas don't do anything anymore.  */

#pragma segment foo

int
main ()
{
  int x, z;  /* { dg-warning "unused variable 'z'" } */
  #pragma unused (x, y)

#ifndef __LP64__
  if (sizeof (struct s1) != 6)
    abort ();
#endif
  if (sizeof (struct s2) != 8)
    abort ();
#ifndef __LP64__
  if (sizeof (struct s3) != 6)
    abort ();
#endif
  if (sizeof (struct s4) != 8)
    abort ();
  return 0;
}

void
unused_err_test ()
{
  int a, b;
  /* Trying to match on '(' or ')' gives regexp headaches, use . instead.  */
#pragma unused  /* { dg-warning "missing '.' after '#pragma unused" } */
#pragma unused (a  /* { dg-warning "missing '.' after '#pragma unused" } */
#pragma unused (b) foo /* { dg-warning "junk at end of '#pragma unused'" } */
}
