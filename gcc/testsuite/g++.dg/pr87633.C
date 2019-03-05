/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

class a {
public:
  double b() const;
};
class c {
public:
  int m_fn2() const;
};
double a::b() const {
  return 0 == 0 ? reinterpret_cast<const c *>(this)->m_fn2() : 0;
}
bool d;
void e() {
  a f;
  double g = f.b();
  /* { dg-final { scan-tree-dump-not "unord" "optimized" } } */
  d = __builtin_isnan(g);
}
