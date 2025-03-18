// { dg-do compile }
// { dg-options "-std=c++23 -fcontracts -fcontracts-nonattr -fcontracts-nonattr-inheritance-mode=P3653 " }

struct NTClass {
  //TODO, make non trivial when https://github.com/NinaRanns/gcc/issues/21 is solved
//  NTClass(){};
//  ~NTClass(){};
};

template <typename... ARGS>
bool check(ARGS... args){ return true;}


struct Base
{
  virtual void f (const NTClass i)  post (check (i));
  virtual void f (const int i)  post (check (i));


}  ;

struct Derived : Base
{
  void f (NTClass i); // { dg-error "used in a postcondition must be const" }
};

struct DerivedV : virtual Base
{
  void f (NTClass i); // { dg-error "used in a postcondition must be const" }
};

template<typename T>
struct DerivedT : Base
{
  void f (NTClass i) {};

  void f (T i) {};
};

template<typename T>
  struct DerivedTV : virtual Base
  {
    void f (NTClass i) {};

    void f (T i) {};
  };

template<typename T>
struct BaseT
{
  virtual void f (const NTClass i) post (check (i));

  virtual void f (const T i) post (check (i));
};

template<typename T>
struct DerivedTT : BaseT<T>
{
  void f (NTClass i) {};

  void f (T i) {};
};

template <typename T>
struct DerivedTVT : virtual BaseT<T>
{
  void f (NTClass i) {};

  void f (T i) {};
};

// adding DerivedT2 for diagnostic disambiguation purposes
template<typename T>
struct DerivedT2 : Base
{
  void f (NTClass i) {};

  void f (T i) {};
};

// adding DerivedTV2 for diagnostic disambiguation purposes
template<typename T>
struct DerivedTV2 : virtual Base
{
  void f (NTClass i) {};

  void f (T i) {};
};

template<typename T>
struct DerivedTT2 : BaseT<T>
{
  void f (NTClass i) {};

  void f (int i) {};
};

template <typename T>
struct DerivedTVT2 : virtual BaseT<T>
{
  void f (NTClass i) {};

  void f (int i) {};
};


int main()
{
  DerivedT<int>  dt;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 35 }
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 37 }

  DerivedTV<int>  dvt;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 43 }
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 45 }


  DerivedTT<int>  dt2;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 59 }
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 61 }

  DerivedTVT<int>  dvt2;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 67 }
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 69 }


  DerivedT2<char>  dtc;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 76 }

  DerivedTV2<char>  dvtc;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 85 }

  DerivedTT2<char>  dtc2;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 93 }

  DerivedTVT2<char>  dvtc2;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 101 }

}
