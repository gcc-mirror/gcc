// PR tree-optimization/60002
// { dg-do compile }
// { dg-options "-O2 -fcompare-debug -fdeclone-ctor-dtor -fipa-cp-clone" }
// { dg-xfail-if "" { powerpc-ibm-aix* } }

struct A {};

struct B : virtual A { B (); ~B (); };

B::~B ()
{
  B ();
}
