// PR c++/82414
// { dg-lto-do link }
enum vals {aa,cc}; // { dg-lto-warning "6: type 'vals' violates the C\\+\\+ One Definition Rule" }
// { dg-lto-note "name 'cc' differs from name 'bb' defined in another translation unit" "" { target *-*-* } .-1 }
struct a { // { dg-lto-warning "8: type 'struct a' violates the C\\+\\+ One Definition Rule" }
  struct b *ptr; // { dg-lto-note "13: the first difference of corresponding definitions is field 'ptr'" }
  // { dg-lto-note "the incompatible type defined in another translation unit" "" { target *-*-* } .-1 }
  enum vals vals;
};
void test(struct a *a)
{
  a->vals = cc;
}
