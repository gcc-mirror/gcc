/* Test function declarations without prototypes for C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

void f1 ();
void
f1a (void)
{
  f1 (1, 2);
}

void f2 ();
void f2 (int);

void f3 ();

_Static_assert (_Generic (f3,
			  void (*) (int) : 1,
			  default : 3) == 1, "unprototyped test");
