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
  virtual void f (const NTClass i);

}  ;

struct Derived : Base
{
  virtual void f (const NTClass i)  post (check (i));
  virtual void g (const NTClass i) pre ( check (i)) post (true);
};

struct DerivedV : virtual Base
{
  virtual void f (const NTClass i)  post (check (i));
  virtual void g (const NTClass i) pre ( check (i)) post (true);
};

void
Derived::f (NTClass i){} // { dg-error "used in a postcondition must be const" }

void
DerivedV::f (NTClass i){} // { dg-error "used in a postcondition must be const" }

void
Derived::g (NTClass i){}

void
DerivedV::g (NTClass i){}
