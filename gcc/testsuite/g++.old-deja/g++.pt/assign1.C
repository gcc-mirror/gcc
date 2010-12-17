// { dg-do compile  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S {			// { dg-error "const|operator=" }
  S();
  T t;
};

void f()
{
  S<const int> s;
  s = s; // { dg-message "synthesized|deleted" }
}
