/* PR c++/34059 */
/* { dg-do compile } */

struct A
{
  template<int> void foo();
};
struct B : A {};
struct C : A {};

class AA
{
  template<int> void foo(); /* { dg-message "private" } */
};
struct BB : AA {};

class AAA {
  int get() const {}
};
struct BBB {
  static BBB *foo();
private:
  int get() const {} /* { dg-message "private" } */
};
template<bool> struct S {
  S(unsigned int = BBB::foo()->AAA::get()); /* { dg-error "is not a base of" } */
};
template<bool> struct SS {
  SS(unsigned int = BBB::foo()->get()); /* { dg-error "within this context" } */
};

void bar()
{
  B().C::foo<0>(); /* { dg-error "is not a member of" } */
  BB().AA::foo<0>(); /* { dg-error "within this context" } */

  int i;
  i.C::foo<0>(); /* { dg-error "which is of non-class type" } */

  S<false> s; /* { dg-error "default argument" } */
  SS<false> ss;
}
