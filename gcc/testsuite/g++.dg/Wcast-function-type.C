/* { dg-do compile } */
/* { dg-options "-Wcast-function-type" } */

struct S
{
  void foo (int*);
  void bar (int);
};

typedef void (S::*MF)(int);

void
foo (void)
{
  MF p1 = (MF)&S::foo; /* { dg-warning "11:cast between incompatible pointer to member" } */
  MF p2 = (MF)&S::bar; /* { dg-bogus   "pointer to member" } */
}
