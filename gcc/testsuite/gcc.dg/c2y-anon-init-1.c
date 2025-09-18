/* Test initialization of anonymous structures and unions (clarified in C2y by
   N3451).  */
/* { dg-do run } */
/* { dg-options "-std=c2y -pedantic-errors" } */

struct s { struct { int a, b; }; union { int c; }; } x = { 1, 2, 3 };
union u { struct { int x, y; }; };

struct s a = { 1, 2, 3 };
union u b = { 4, 5 };

extern void abort ();
extern void exit (int);

int
main ()
{
  if (a.a != 1 || a.b != 2 || a.c != 3)
    abort ();
  if (b.x != 4 || b.y != 5)
    abort ();
  exit (0);
}
