/* { dg-do compile } */
/* { dg-options "-Wno-old-style-definition -O2 -Winline -fopt-info-optimized-inline=stderr" } */

struct s { int a; };

inline void f (x)
     int x;
{
  asm ("");
}

void g (struct s x)
{
  f (x); 		/* { dg-optimized "Inlining f.* into g" } */
}

void f (int x);		/* { dg-warning "follows non-prototype definition" } */
