// I, Howard Hinnant, hereby place this code in the public domain.

// Test that the implicit object parameter is *not* an rvalue reference, but is instead
//   identical to that specified in C++03.  That is, the implicit object parameter is
//   an lvalue reference that can bind to an rvalue. :-\
//   See http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2006/n2118.html under the
//   section "Revision 1 Summary and Rationale" for more details.

// { dg-do compile }
// { dg-options "-std=c++0x" }

template <bool> struct sa;
template <> struct sa<true> {};

struct one   {char x[1];};
struct two   {char x[2];};

struct os
{
    one operator<<(int);
};

struct A
{
    A(int);
};

two operator<<(os&, const A&);

void test()
{
    os o;
    sa<sizeof(o << 1) == 1> t1;  // Calls os::operator<<(int)
                                 // Would be ambiguous if the implicit object parameter
                                 // was an rvalue reference.
}

int main()
{
    return 0;
}
