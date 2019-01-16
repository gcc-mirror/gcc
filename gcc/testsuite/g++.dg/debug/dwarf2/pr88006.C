// { dg-additional-options "-dA -std=gnu++17 -gdwarf-4 -O1 -fdebug-types-section" }
// reject .pseudo label, but "label" is ok.
// { dg-final { scan-assembler-not "\[^\"\]_ZN3Foo4mfunEv" } }
// undefined ref to _ZN3Foo4mfunEv

struct Foo {
  void mfun () {}
};

struct A { static constexpr bool Value = false; };

template <bool> struct B { typedef int Type; };

class Arg
{
  template <typename Unused> struct Local : A {};

public:
  template <typename Init, typename = typename B<Local<Init>::Value>::Type>
  Arg (Init) {}
};

class Lambda {
  static constexpr int Unused = 0;
    
public:
  Lambda (Arg);
};

// Generated ref to Foo::mfun in the type die of an instantiation of this
template <void (Foo::*unused)()> struct Callable {};

class I {
  I() : lamb ([this] {}) {}

  Lambda lamb;

  Callable<&Foo::mfun> bm;
};
