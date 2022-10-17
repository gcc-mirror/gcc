/* PR middle-end/88232 - Please implement -Winfinite-recursion
   { dg-do compile }
   { dg-options "-Wall -Winfinite-recursion" } */

template <typename D>
struct C
{
  void foo ()                       // { dg-warning "-Winfinite-recursion" }
  {
    static_cast<D *>(this)->foo ();
  }
};

struct D : C<D>
{
  // this is missing:
  // void foo() {}
};

void f (D *d)
{
  d->foo ();
}


struct E : C<D>
{
  void foo() {}
};

void g (E *e)
{
  e->foo ();
}
