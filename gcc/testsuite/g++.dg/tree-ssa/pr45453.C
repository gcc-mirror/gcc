/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fno-devirtualize-speculatively" } */
struct S
{
  S();
  virtual inline void foo ()
  {
    foo();
  }
};

void
B ()
{
  S().foo ();
}
/* We should inline foo and devirtualize call to foo in the inlined version.  */
// { dg-final { scan-tree-dump-times "OBJ_TYPE_REF" 1 "optimized" } }
