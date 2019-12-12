// PR c++/86900
// { dg-do assemble { target function_sections } }
// { dg-options "-O2 -gdwarf-5 -ffunction-sections" }
// { dg-xfail-if "" { powerpc-ibm-aix* } }

struct A;
struct B { B (A); };
struct A { A (int); ~A (); };

void
foo (int x)
{
  A d(0);
  B e(d);
}
