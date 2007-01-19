/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-tree_profile" } */

struct A {
  A () {}

  virtual int AA (void)
  { return 0; }

};

struct B : public A {
  B () {}

  virtual int AA (void)
  { return 1; }
};

int
main (void)
{
  A a;
  B b;
  
  A* p;

  p = &a;
  p->AA ();

  p = &b;
  p->AA ();
  
  return 0;
}

/* { dg-final-use { scan-tree-dump "Indirect call -> direct call.* AA transformation on insn" "tree_profile"} } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */                                                                                
/* { dg-final-use { cleanup-tree-dump "optimized" } } */                                                                                              
/* { dg-final-use { cleanup-tree-dump "tree_profile" } } */                                                                                           

