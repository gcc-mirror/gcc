// { dg-do compile }
// { dg-options "-g -O0 -dA" }

struct C {  // { dg-function-on-line {_ZN1CC[12]Ev} }
  int i;
  virtual void
  foo() {}  // { dg-function-on-line _ZN1C3fooEv }
};
static C dummy;

int
main (void)
{  // { dg-function-on-line main }
}
