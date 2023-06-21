// PR c++/109321
// { dg-do compile { target c++20 } }

struct A;
template<class C1> struct B;
template<class, class C2=A> using D = B<C2>;
void f () { D() = 0; }		// { dg-error "deduction failed|no match" }
