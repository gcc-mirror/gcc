// { dg-do compile }
// { dg-options "-gdwarf-2 -O0 -dA" }

struct C {  // { dg-function-on-line {_ZN1CC[12]Ev} { xfail { powerpc-ibm-aix* } } }
  int i;
  virtual void
  foo() {}  // { dg-function-on-line _ZN1C3fooEv { xfail { powerpc-ibm-aix* } } }
};
static C dummy;

int
main (void)
{  // { dg-function-on-line main { xfail { powerpc-ibm-aix* } } }
}
