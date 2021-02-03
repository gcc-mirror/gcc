// PR c++/98515
// { dg-do compile }

struct A { protected: int var0; };
template <class> struct B : public A { };
template <class T> struct C : public B<T> { void g(); };
template <class T> void C<T>::g() { A::var0++; }
template class C<int>;
