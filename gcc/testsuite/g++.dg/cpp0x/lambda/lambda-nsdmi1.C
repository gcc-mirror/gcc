// PR c++/56464
// { dg-do run { target c++11 } }

struct bug { bug*a = [&]{ return [=]{return this;}(); }(); };
int main()
{
  bug b;
  if (b.a != &b)
    __builtin_abort ();
}
