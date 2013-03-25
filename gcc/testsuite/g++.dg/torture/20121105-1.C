// PR tree-optimization/54986
// Reported by Remi Vanicat <vanicat@debian.org>
// Reduced testcase by Markus Trippelsdorf <markus@trippelsdorf.de> 

__extension__ typedef __INTPTR_TYPE__ intptr_t;

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
const intptr_t d = (intptr_t)&c;
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
