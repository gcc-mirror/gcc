/* Test C2x variadic functions with no named parameters, or last named
   parameter with a declaration not allowed in C17.  Execution tests split
   between source files.  */
/* { dg-do run } */
/* { dg-options "-std=c2x -pedantic-errors" } */
/* { dg-additional-sources "c2x-stdarg-split-1b.c" } */

extern void abort (void);
extern void exit (int);

double f (...);
void g (...);
void h1 (register int x, ...);
void h2 (int x(), ...);
void h3 (int x[10], ...);
void h4 (char x, ...);
void h5 (float x, ...);
void h6 (volatile long x, ...);
struct s { char c[1000]; };
void h7 (volatile struct s x, ...);

int
main ()
{
  if (f (1, 2.0, 3, 4.0) != 10.0)
    abort ();
  g (0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0);
  g (0.0f, 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f);
  h1 (0, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  h2 (0, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  h3 (0, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  h4 (0, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  h5 (0, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  h6 (0, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  h7 ((struct s) {}, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  exit (0);
}
