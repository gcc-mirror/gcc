// PR c++/82414
// { dg-lto-do link }
struct a { // { dg-lto-warning "8: type 'struct a' violates the C\\+\\+ One Definition Rule" }
  struct b *ptr; // { dg-lto-message "13: the first difference of corresponding definitions is field 'ptr'" }
};
void test(struct a *)
{
}
