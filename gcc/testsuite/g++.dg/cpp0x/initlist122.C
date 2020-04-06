// PR c++/94041
// { dg-do run { target c++11 } }

bool gone;
struct Temp { ~Temp() { gone = true; } };
struct A{ A() {}; A(const Temp&) noexcept {};  };
struct B{ ~B() {}; };
struct Pair{ A a; B b; };

void foo(const Pair&) noexcept { if (gone) __builtin_abort(); }

int main()
{
  foo({A(Temp{}), B()});
  if (!gone) __builtin_abort ();
}
