// PR tree-optimization/54986
// Reported by Remi Vanicat <vanicat@debian.org>
// Reduced testcase by Markus Trippelsdorf <markus@trippelsdorf.de> 

struct A;
struct B
{
  int *_ptr;
  bool operator==(B *p1)
  {
    return p1->_ptr;
  }
};
struct C {
  A* ref_SYMBptr();
};
struct A
{
  B sommet;
};
typedef C *gen_op_context;
struct D
{
  D(gen_op_context) {}
};

D c(0);
const long d = (long)&c;
B *const   e = (B *)&d;

static bool
fn1(C& p1)
{
  return p1.ref_SYMBptr()->sommet == e;
}

void
fn2()
{
  C b;
  fn1(b);
}
