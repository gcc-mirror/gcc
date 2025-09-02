// { dg-do compile { target c++26 } }
// Test consteval blocks, as specified by P2996.

void fn ();

consteval { fn (); }	    // { dg-error "call to non-.constexpr. function" }
consteval { return 42; }    // { dg-error "return-statement with a value" }

struct S {
  consteval {
    fn ();		    // { dg-error "call to non-.constexpr. function" }
  }
  consteval {
    return 42;		    // { dg-error "return-statement with a value" }
  }
};

template<typename T>
constexpr void foo (T t) { return t; }	// { dg-error "return-statement with a value" }

template<int N>
struct R {
  consteval { foo (N); }
};

R<1> r;

template<typename T>
constexpr void foo2 (T t) { return t; }	// { dg-error "return-statement with a value" }

template<int N>
void
f ()
{
  consteval { foo2 (1); }
}

constexpr int bar (int) { return 0; }

void
g ()
{
  f<1>();

  int r = 42;
  consteval {
    bar (r);	      // { dg-error ".r. is not captured" }
  }
}
