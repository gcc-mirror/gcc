// PR c++/94041
// { dg-do run { target c++11 } }

bool gone;
struct Temp { ~Temp() { gone = true; } };
struct A{ A() {}; A(const Temp&) noexcept {};  };
struct B{ ~B() {}; };
struct Pair{ A a; B b; };
void foo(const Pair&) noexcept { if (gone) __builtin_abort(); }

B bar() { if (gone) __builtin_abort(); return {}; }

int main()
{
  Pair p{A(Temp{}), bar()};

  if (!gone) __builtin_abort ();

  gone = false;

  foo({A(Temp{})});

  if (!gone) __builtin_abort ();
}
