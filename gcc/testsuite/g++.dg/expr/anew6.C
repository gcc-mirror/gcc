// PR c++/97523
// { dg-do compile { target c++11 } }

// [expr.new]/24: If the new-expression creates an object or an array of
// objects of class type, access and ambiguity control are done for the
// [...] constructor selected for the initialization (if any).
// NB: We only check for a default constructor if the array has a non-constant
// bound, or there are insufficient initializers.  Since an array is an
// aggregate, we perform aggregate-initialization, which performs
// copy-initialization, so we only accept converting constructors.

struct T {
  explicit T();
  T(int);
};

struct S {
  S(int);
};

void
fn (int n)
{
  new T[1]{}; // { dg-error "explicit constructor" }
  new T[2]{1, 2};
  new T[3]{1, 2}; // { dg-error "explicit constructor" }
  new T[n]{}; // { dg-error "explicit constructor" }

  new S[1]{}; // { dg-error "could not convert" }
  new S[2]{1, 2};
  new S[3]{1, 2}; // { dg-error "could not convert" }
  new S[n]{}; // { dg-error "could not convert" }
}
