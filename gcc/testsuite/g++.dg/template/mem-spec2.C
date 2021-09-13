// PR c++/94529

template <class T>
struct foo {
    // the issue is const here
    void bar(T& foobar) const { foobar = 0; } // { dg-message "candidate" }
};

template <> void
foo<int>::bar(int& foobar) { foobar = 9; } // { dg-error "does not match" }
// { dg-bogus "member function template" "" { target *-*-* } .-1 }
