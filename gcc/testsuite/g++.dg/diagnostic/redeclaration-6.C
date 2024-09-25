// PR c++/116239
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fno-implicit-constexpr" }

consteval void f1();
void f2();
constexpr void f3();
void f4();
consteval void f5();
constexpr void f6();

void
g ()
{
  void f1();		// { dg-error "differs in .consteval." }
  consteval void f2();	// { dg-error "differs in .consteval." }

  void f3();		// { dg-error "differs in .constexpr." }
  constexpr void f4();  // { dg-error "differs in .constexpr." }

  consteval void f5();
  constexpr void f6();

  void f7();
  consteval void f7();	// { dg-error "differs in .consteval." }

  consteval void f8();
  void f8();		// { dg-error "differs in .consteval." }

  void f9();
  constexpr void f9();	// { dg-error "differs in .constexpr." }

  constexpr void f10();
  void f10();		// { dg-error "differs in .constexpr." }
}
