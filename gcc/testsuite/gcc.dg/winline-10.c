/* { dg-do compile } */
/* { dg-options "-O2 -Winline" } */

struct s { int a; };

inline void f (x)	/* { dg-warning "inlining .* mismatched arg" } */
     int x;
{
  asm ("");
}

void g (struct s x)
{
  f (x); 		/* { dg-message "called from here" } */
}

void f (int x);		/* { dg-warning "follows non-prototype definition" } */
