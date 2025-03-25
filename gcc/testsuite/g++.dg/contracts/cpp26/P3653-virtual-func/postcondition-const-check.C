// { dg-do compile }
// { dg-options "-std=c++23 -fcontracts -fcontracts-nonattr -fcontracts-on-virtual-functions=P3653 " }

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
  virtual void g (const NTClass i, int& j) pre ( check (i)) post (check(j));
}  ;

struct Derived : Base
{
  void f (NTClass i); // { dg-error "used in a postcondition must be const" }

  void g (NTClass i, int& j);
};

struct DerivedV : virtual Base
{
  void f (NTClass i); // { dg-error "used in a postcondition must be const" }

  void g (NTClass i, int& j);
};

template<typename T>
struct DerivedT : Base
{
  void f (NTClass i) {};

  void f (T i) {};

  void g (NTClass i, T& j);
};

template<typename T>
  struct DerivedTV : virtual Base
  {
    void f (NTClass i) {};

    void f (T i) {};

    void g (NTClass i, T& j);
  };

template<typename T>
struct BaseT
{
  virtual void f (const NTClass i) post (check (i));

  virtual void f (const T i) post (check (i));

  virtual void g (const NTClass i, T& j) pre ( check (i)) post (check(j));
};

template<typename T>
struct DerivedTT : BaseT<T>
{
  void f (NTClass i) {};

  void f (T i) {};

  void g (NTClass i, T& j);
};

template <typename T>
struct DerivedTVT : virtual BaseT<T>
{
  void f (NTClass i) {};

  void f (T i) {};

  void g (NTClass i, T& j);
};

// adding DerivedT2 for diagnostic disambiguation purposes
template<typename T>
struct DerivedT2 : Base
{
  void f (NTClass i) {};

  void f (T i) {};

  void g (NTClass i, T& j);
};

// adding DerivedTV2 for diagnostic disambiguation purposes
template<typename T>
struct DerivedTV2 : virtual Base
{
  void f (NTClass i) {};

  void f (T i) {};

  void g (NTClass i, T& j);
};

template<typename T>
struct DerivedTT2 : BaseT<T>
{
  void f (NTClass i) {};

  void f (int i) {};

  void g (NTClass i, T& j);
};

template <typename T>
struct DerivedTVT2 : virtual BaseT<T>
{
  void f (NTClass i) {};

  void f (int i) {};

  void g (NTClass i, T& j);
};


int main()
{
  DerivedT<int>  dt;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 38 }
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 40 }

  DerivedTV<int>  dvt;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 48 }
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 50 }


  DerivedTT<int>  dt2;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 68 }
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 70 }

  DerivedTVT<int>  dvt2;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 78 }
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 80 }


  DerivedT2<char>  dtc;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 89 }

  DerivedTV2<char>  dvtc;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 100 }

  DerivedTT2<char>  dtc2;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 110 }

  DerivedTVT2<char>  dvtc2;
  // { dg-error {used in a postcondition must be const} "" { target *-*-* } 120 }

}
