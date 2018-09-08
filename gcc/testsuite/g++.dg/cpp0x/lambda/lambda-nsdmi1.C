// PR c++/56464
// { dg-do run { target c++11 } }

struct bug { bug*a = [&]{ return [=]{return this;}(); }(); }; // { dg-warning "implicit capture" "" { target c++2a } }
int main()
{
  bug b;
  if (b.a != &b)
    __builtin_abort ();
}
