// { dg-options "-O3 -fdump-tree-ssa" }
inline void t()
{
  struct A {virtual void q() {}};
  static struct A *a;
  if (!a)
    a = new(A);
  a->q();
};
void
m()
{
  t();
}
// { dg-final { scan-tree-dump-not "OBJ_TYPE_REF" "ssa" } }
