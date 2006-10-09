/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target tls } */

extern void abort (void);
extern void exit (int);

struct A
{
  char a;
  int b;
  long long c;
};
extern __thread struct A a1, a2, a3, a4;
extern struct A *f1a (void);
extern struct A *f2a (void);
extern struct A *f3a (void);
extern struct A *f4a (void);
extern struct A *f5a (void);
extern struct A *f6a (void);
extern struct A *f7a (void);
extern struct A *f8a (void);
extern struct A *f9a (void);
extern struct A *f10a (void);
extern int f1b (void);
extern int f2b (void);
extern int f3b (void);
extern int f4b (void);
extern int f5b (void);
extern int f6b (void);
extern int f7b (void);
extern int f8b (void);
extern int f9b (void);
extern int f10b (void);
extern void check1 (void);
extern void check2 (void);
__thread int dummy = 12;
__thread struct A local = { 1, 2, 3 };

int
main (void)
{
  struct A *p;

  if (local.a != 1 || local.b != 2 || local.c != 3)
    abort ();
  if (a1.a != 4 || a1.b != 5 || a1.c != 6)
    abort ();
  if (a2.a != 22 || a2.b != 23 || a2.c != 24)
    abort ();
  if (a3.a != 10 || a3.b != 11 || a3.c != 12)
    abort ();
  if (a4.a != 25 || a4.b != 26 || a4.c != 27)
    abort ();
  check1 ();
  check2 ();
  if (f1a () != &a1 || f2a () != &a2 || f3a () != &a3 || f4a () != &a4)
    abort ();
  p = f5a (); if (p->a != 16 || p->b != 16 + 1 || p->c != 16 + 2)
    abort ();
  p = f6a (); if (p->a != 19 || p->b != 19 + 1 || p->c != 19 + 2)
    abort ();
  if (f7a () != &a2 || f8a () != &a4)
    abort ();
  p = f9a (); if (p->a != 28 || p->b != 28 + 1 || p->c != 28 + 2)
    abort ();
  p = f10a (); if (p->a != 31 || p->b != 31 + 1 || p->c != 31 + 2)
    abort ();

  exit (0);
}
