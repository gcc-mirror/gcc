// PR c++/82414
// { dg-lto-do link }
enum vals {aa,cc}; // { dg-lto-warning "6: type 'vals' violates the C\\+\\+ One Definition Rule" }
struct a { // { dg-lto-warning "8: type 'struct a' violates the C\\+\\+ One Definition Rule" }
  struct b *ptr; // { dg-lto-message "13: the first difference of corresponding definitions is field 'ptr'" }
  enum vals vals;
};
void test(struct a *a)
{
  a->vals = cc;
}
