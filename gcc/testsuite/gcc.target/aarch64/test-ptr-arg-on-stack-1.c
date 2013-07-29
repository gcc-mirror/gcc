/* { dg-do run } */
/* { dg-options "-O2 -fno-inline" } */

/* Test pass-by-reference and pointer-typed argument passing on stack.
   This test shall pass on any of the following four combinitions:
    {big-endian, little-endian} {LP64, ILP32}.  */

struct s5
{
  double a;
  double b;
  double c;
  double d;
  double e;
} gS = {1.0, 2.0, 3.0, 4.0, 5.0};

double  __attribute__ ((noinline))
foo (struct s5 p1, struct s5 p2, struct s5 p3, struct s5 p4,
     struct s5 p5, struct s5 p6, struct s5 p7, struct s5 p8,
     struct s5 p9)
{
  asm ("");
  return p9.c;
}

void abort (void);
int printf (const char *, ...);

int main (void)
{
  printf ("Here we print out some values and more importantly hope that"
	  " the stack is getting a bit dirty for the bug to manifest itself"
	  "\n\t%f, %f, %f, %f, %f\n", gS.a, gS.b, gS.c, gS.d, gS.e);

  if (foo (gS, gS, gS, gS, gS, gS, gS, gS, gS) != 3.0)
    abort ();

  return 0;
}
