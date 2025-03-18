// { dg-do compile }
// { dg-options "-std=c++23 -fcontracts -fcontracts-nonattr -fcontracts-nonattr-inheritance-mode=P2900R13 " }

struct NTClass {
  //TODO, make non trivial when https://github.com/NinaRanns/gcc/issues/21 is solved
//  NTClass(){};
//  ~NTClass(){};
};

template <typename... ARGS>
bool check(ARGS... args){ return true;}


struct Base
{
  virtual void f (const NTClass i);

}  ;

struct Derived : Base
{
  virtual void f (const NTClass i)  post (check (i));
};

struct DerivedV : virtual Base
{
  virtual void f (const NTClass i)  post (check (i));
};

void
Derived::f (NTClass i){} // { dg-error "used in a postcondition must be const" }

void
DerivedV::f (NTClass i){} // { dg-error "used in a postcondition must be const" }
