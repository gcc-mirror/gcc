/* Test that declaring a function with () is the same as (void) in C23.  Valid
   use cases.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wstrict-prototypes" } */

void f1 ();
void f1 (void);

void f2 (void);
void f2 ();

typedef void T1 ();
typedef void T1 (void);

void f3 ();

_Static_assert (_Generic (f3,
			  void (*) (int) : 1,
			  void (*) (void) : 2,
			  default : 3) == 2);
