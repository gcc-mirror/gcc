// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test various forms of member access with this.

struct S {
  int i;
  static constexpr int j = 42;
  int foo () { return 42; }

  void
  f ()
  {
    static_assert([: ^^ j :] == 42);
    static_assert([: ^^S :]::j == 42);
    int q = this->[: ^^i :];
    q += this->[: ^^S :]::i;
    this->[: ^^foo :] ();
    this->[: ^^S :]::foo ();
  }
};

struct D {
  int i;
  static constexpr int j = 42;
  int foo () { return 42; }

  template<typename T>
  void
  f ()
  {
    static_assert([: ^^T :]::j == 42);
    int q = this->[: ^^T :]::j;
    this->[: ^^T :]::foo ();
  }
};
