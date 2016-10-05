// { dg-do compile { target c++11 } }
// { dg-options "" }

extern int foo (void);
extern void bar (int);

void
f (void)
{
  if (auto p = foo (); p > 10) // { dg-warning "init-statement" "" { target c++14_down } }
    bar (p);
  else
    bar (-p);
}
// { dg-do compile { target c++11 } }
// { dg-options "" }

extern int foo (void);
extern void bar (int);

void
f (void)
{
  if (auto p = foo (); p > 10) // { dg-warning "init-statement" "" { target c++14_down } }
    bar (p);
  else
    bar (-p);
}
