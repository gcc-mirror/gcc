// PR c++/14510

struct c {};
namespace A {
  int c(struct c*req);
}
int A::c(struct c*req) { return 0; }
