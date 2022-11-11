// PR c++/101165 - P2266R1 - Simpler implicit move
// { dg-do compile { target c++23 } }
// { dg-options "-Wdangling-reference" }
// Test from P2266R1, $ 5.2. LibreOffice OString constructor.

struct X {
    X(auto&);
};

// The following compiles in C++20 (deducing X(char (&)[10])) but not
// after P2266 (because the returned expression now has type char (&&)[10],
// which cannot bind to auto&).
X f() {
    char a[10];
    return a; // { dg-error "cannot bind non-const lvalue reference" }
}

// The solution was to change it by making the return convert explicitly
// rather than implicitly:
X fixed() {
    char a[10];
    return X(a);
}

// $ 5.3. LibreOffice o3tl::temporary

template<class T>
T& temporary1(T&& x) { return x; } // { dg-error "cannot bind non-const lvalue reference" }

// Fixed by:
template<class T>
T& temporary2(T&& x) { return static_cast<T&>(x); }

void
test ()
{
  int& r1 = temporary1 (42);
  int& r2 = temporary2 (42);
}
