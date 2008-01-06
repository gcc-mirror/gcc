// { dg-do compile }

struct A { virtual ~A() { }; };
struct B : A { };

void f(B* bp)
{
  bp =
#ifdef __GXX_RTTI
  dynamic_cast<B*>(static_cast<A*>(0));
#endif
}
