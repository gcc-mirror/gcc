// { dg-options "-fno-pretty-templates" }

template<typename T>
  struct A
  {
    template<typename U>
      void f() {} // { dg-line Af }

    template<typename U>
      void g(U) {} // { dg-line Ag }
  };

template<typename T>
  struct B
  {
    template<typename U>
      void f(U) {} // { dg-line Bf }

    template<typename U>
      void g(U, T) {} // { dg-line Bg }
  };

struct C
{
  template<typename U>
    void f(U) {} // { dg-line Cf }

  template<typename U>
    void g() {} // { dg-line Cg }
};

int main()
{
  A<int>().f(0); // { dg-error "no matching function for call to 'A<int>::f\\(int\\)'" }
  // { dg-message "candidate: 'template<class U> void A<int>::f\\(\\)'" "" { target *-*-* } Af }

  A<int>().g(); // { dg-error "no matching function for call to 'A<int>::g\\(\\)'" }
  // { dg-message "candidate: 'template<class U> void A<int>::g\\(U\\)'" "" { target *-*-* } Ag }

  B<int>().f(); // { dg-error "no matching function for call to 'B<int>::f\\(\\)'" }
  // { dg-message "candidate: 'template<class U> void B<int>::f\\(U\\)'" "" { target *-*-* } Bf }

  B<int>().g(); // { dg-error "no matching function for call to 'B<int>::g\\(\\)'" }
  // { dg-message "candidate: 'template<class U> void B<int>::g\\(U, int\\)'" "" { target *-*-* } Bg }

  B<float>().g(0); // { dg-error "no matching function for call to 'B<float>::g\\(int\\)'" }
  // { dg-message "candidate: 'template<class U> void B<float>::g\\(U, float\\)'" "" { target *-*-* } Bg }

  C().f(); // { dg-error "no matching function for call to 'C::f\\(\\)'" }
  // { dg-message "candidate: 'template<class U> void C::f\\(U\\)'" "" { target *-*-* } Cf }

  C().g(0); // { dg-error "no matching function for call to 'C::g\\(int\\)'" }
  // { dg-message "candidate: 'template<class U> void C::g\\(\\)'" "" { target *-*-* } Cg }
}
