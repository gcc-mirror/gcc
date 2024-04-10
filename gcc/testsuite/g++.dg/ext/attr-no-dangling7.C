// PR c++/110358
// { dg-do compile { target c++20 } }
// { dg-options "-Wdangling-reference" }

class X { };
const X x1;
const X x2;

template<bool... N>
[[gnu::no_dangling(N)]] const X& get(const int& i); // { dg-error "parameter packs not expanded" }

template<typename T>
[[gnu::no_dangling(T::x)]] // { dg-error "member" }
const X& foo(const int& i);

bool val () { return true; }

[[gnu::no_dangling(val ())]]   // { dg-error "call" }
const X& bar (const int& i);

[[gnu::no_dangling(20)]] const X& fn1 (const int &);

void
test ()
{
  [[maybe_unused]] const X& x1 = bar (10);	  // { dg-warning "dangling" }
  [[maybe_unused]] const X& x2 = foo<int> (10);	  // { dg-error "no matching" }
  [[maybe_unused]] const X& x3			  // { dg-warning "dangling" }
    = fn1 (10);					  // { dg-error "narrowing" }
}

