// PR c++/29273

struct A { virtual ~A () { } };
struct B: A { } b [1];

void foo ()
{
  dynamic_cast<B*>(b);
}
