// PR c++/70627
// { dg-do compile { target c++11 } }

struct D;
struct A
{
  D *operator->();
};
struct B
{
  template <typename... T> void foo (T &&...) {}
};
typedef unsigned char G;
enum class H : G;
struct C
{
};
struct D : C
{
  B foo () const { B a; a.foo (d); }
  H d;
};
struct F : C
{
  void foo ();
  A f;
};
enum class H : unsigned char;
void F::foo () { B b = f->foo (); }
