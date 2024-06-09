// ABI #47 "natural" template parameter mangling
// { dg-do compile { target c++17 } }
// { dg-additional-options -fabi-compat-version=0 }

template <template <class...> class TT> class A { };
template <int... T> class B { };

template <auto... T>
void f(B<T...> b);

template <template <auto...> class TT>
void g(TT<42>);

template <template <int...> class TT>
void h(TT<42>);

template <class T> struct C {
  template <template <T...> class TT> static void j(TT<42>);
};

int main()
{
  B<42> b;
  f(b); // { dg-final { scan-assembler "_Z1fITpTnDaJLi42EEEv1BIJXspT_EEE" } }
  g(b); // { dg-final { scan-assembler "_Z1gITtTpTnDaE1BEvT_IJLi42EEE" } }
  h(b); // { dg-final { scan-assembler "_Z1hI1BEvT_IJLi42EEE" } }
  C<int>::j(b); // { dg-final { scan-assembler "_ZN1CIiE1jI1BEEvT_IJLi42EEE" } }
}
