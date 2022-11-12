/* Test C2x constexpr.  Valid code, execution test.  */
/* { dg-do link } */
/* { dg-options "-std=c2x -pedantic-errors" } */
/* { dg-additional-sources "c2x-constexpr-2b.c" } */

extern void abort (void);
extern void exit (int);

/* constexpr objects at file scope have internal linkage.  */
constexpr int a = 2;

struct s { int a; float b; int c[3]; };
constexpr struct s s1 = { 2, 3, { 4, 5, 6 } };
constexpr struct s s2 = s1;
struct s s3 = s2;

void
check (const struct s *p)
{
  if (p->a != 2 || p->b != 3 || p->c[0] != 4 || p->c[1] != 5 || p->c[2] != 6)
    abort ();
}

int
main ()
{
  constexpr struct s s4 = s1;
  struct s s5 = s4;
  constexpr struct s s6 = { s1.a, s2.b, { 4, 5, 6 } };
  check (&s1);
  check (&s2);
  check (&s3);
  check (&s4);
  check (&s5);
  check (&s6);
  exit (0);
}
