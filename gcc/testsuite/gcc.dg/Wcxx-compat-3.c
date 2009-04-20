/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */
enum E1 { A, B, C };
enum E2 { D, E, F };
extern void f2 (enum E1);

void
f1 ()
{
  int a = A;
  enum E1 e1;
  enum E2 e2;

  f2 (0);	/* { dg-warning "invalid in C\[+\]\[+\]" } */
  f2 (A);
  f2 (D);	/* { dg-warning "invalid in C\[+\]\[+\]" } */
  f2 (a);	/* { dg-warning "invalid in C\[+\]\[+\]" } */
  f2 (e1);
  f2 (e2);	/* { dg-warning "invalid in C\[+\]\[+\]" } */
  f2 ((A));
  f2 (a ? A : B);
  f2 ((enum E1) 0);
  f2 ((enum E1) D);
  f2 ((enum E1) a);
  f2 ((enum E1) e2);
}

struct s1 { enum E1 e1 : 3; };
struct s2 { enum E2 e2 : 3; };

void
f3 (struct s1 sv1, struct s2 sv2)
{
  f2 (sv1.e1);
  f2 (sv2.e2);	/* { dg-warning "invalid in C\[+\]\[+\]" } */
}

void
f4 (struct s1 *pv1, struct s2 *pv2)
{
  f2 (pv1->e1);
  f2 (pv2->e2);	/* { dg-warning "invalid in C\[+\]\[+\]" } */
}

/* Match all extra informative notes.  */
/* { dg-message "note: expected '\[^\n'\]*' but argument is of type '\[^\n'\]*'" "note: expected" { target *-*-* } 0 } */
