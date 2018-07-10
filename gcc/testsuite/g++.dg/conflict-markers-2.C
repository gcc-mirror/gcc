// { dg-do compile { target c++11 } }

extern void f1 (void);
extern void f2 (void);
extern void f3 (void);
extern void f4 (void);

void test ()
{
  f1 ();
<<<<<<< HEAD // { dg-error "conflict marker" }
  f2 ();
======= // { dg-error "conflict marker" }
  f3 ();
>>>>>>> 252be53... Some commit message // { dg-error "conflict marker" }
  f4 ();
}
