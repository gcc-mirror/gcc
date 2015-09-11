/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp1 -fdelete-null-pointer-checks" } */
struct t
{
  static inline void tt()
  {
  }
  virtual void q();
};
int m()
{
  void *q = (void *)&t::tt;
  return q != 0;
}
/* { dg-final { scan-tree-dump "return 1" "ccp1"} } */
