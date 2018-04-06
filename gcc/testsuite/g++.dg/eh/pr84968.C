// { dg-do compile { target c++11 } }

// { dg-options "" }

union b;

struct S {
  template <typename>
  void a()
    try {
    } catch (int ()
	     noexcept (({ union b a; true; }))) // { dg-error "constant" }
  {
  }
};
