// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=off -fcontracts-nonattr -fcontracts-nonattr-inheritance-mode=Ville -Wsuggest-explicit-contract" }
#include <cassert>

/*missing bits :
 * - virtual destructor
 * - virtual constructors
 */

bool test(int i){ return true;}

namespace T1{
struct Base
{
  virtual void f1(int i) pre (test(i)){};
  virtual void f2(int i) pre (test(i)){};
  virtual int f3(const int i) post (test(i)){ return i;};
  virtual void f4(const int i) post (test(i)){};
  virtual void f5() {};

};

struct Child :  Base
{
  virtual void f1(int i) {}; // { dg-warning "Function implicitly inherits a contract" }
  virtual void f2(int i) pre (test(i)){}
  virtual int f3(const int i) {return i;};// { dg-warning "Function implicitly inherits a contract" }
  void f4(int i){}; // { dg-warning "Function implicitly inherits a contract" }
  virtual void f5(int i) {};
  virtual void f6(int i) {};

};

struct GChild :  Child
{
  virtual void f1(int i) {}; // { dg-warning "Function implicitly inherits a contract" }
  virtual void f2(int i) pre (test(i)){}
  //virtual int f3(int i) not defined
  void f4(bool b){};
  virtual void f5(int i) {};

};

struct GGChild :  GChild
{
  virtual void f1(int i) {}; // { dg-warning "Function implicitly inherits a contract" }
  virtual void f2(int i) pre (test(i)){}
  virtual int f3(const int i) { return i; }
  void f4(int i){};
  virtual void f5(int i) {};

};
}; //namespace T1

// template base
namespace T2{
template<typename T>
struct Base
{
  virtual void f1(int i) pre (test(i)){};
  virtual void f2(int i) pre (test(i)){};
  virtual int f3(const int i) post (r: test(r)){ return i;};
  virtual void f4(const int i) post (test(i)){};
  virtual void f5(int i) {};
  virtual void f6(int i) {};

};


struct Child : Base<int>
{
  virtual void f1(int i) {}; // { dg-warning "Function implicitly inherits a contract" }
  virtual void f2(int i) pre (test(i)){}
  virtual int f3(int i) {return i;};// { dg-warning "Function implicitly inherits a contract" }
  void f4(int i){};
  virtual void f5(int i) {};
  virtual void f6(int i) {};

};

struct GChild :  Child
{
  virtual void f1(int i) {}; // { dg-warning "Function implicitly inherits a contract" }
  virtual void f2(int i) pre (test(i)){}
  //virtual int f3(int i) not defined
  void f4(int i){};
  virtual void f5(int i) {};

};

struct GGChild :  GChild
{
  virtual void f1(int i) {}; // { dg-warning "Function implicitly inherits a contract" }
  virtual void f2(int i) pre (test(i)){}
  virtual int f3(int i) { return i; }
  void f4(int i){};
  virtual void f5(int i) {};

};

}; //namespace T2


// template base
namespace T3{
template<typename T>
struct BaseT
{
  virtual void f1(int i) pre (test(i)){};
  virtual void f2(int i) pre (test(i)){};
  virtual int f3(int i) post (r: test(r)){ return i;};
  virtual void f4(int i) post (test(i)){};
  virtual void f5(int i) {};
  virtual void f6(int i) {};


};

struct Child2 : virtual Base<int>
{
  virtual void f1(int i) {}; // { dg-warning "Function implicitly inherits a contract" }
  virtual void f2(int i) pre (test(i)){}
  virtual int f3(int i) {return i;};// { dg-warning "Function implicitly inherits a contract" }
  void f4(int i){};
  virtual void f5(int i) {};
  virtual void f6(int i) {};

  virtual ~Child2() {}// { dg-warning "Function implicitly inherits a contract" }
};

struct GChild2 :  Child2
{
  virtual void f1(int i) {}; // { dg-warning "Function implicitly inherits a contract" }
  virtual void f2(int i) pre (test(i)){}
  //virtual int f3(int i) not defined
  void f4(int i){};
  virtual void f5(int i) {};

  virtual ~Child2() {}// { dg-warning "Function implicitly inherits a contract" }
};

struct GGChild2 :  GChild2
{
  virtual void f1(int i) {}; // { dg-warning "Function implicitly inherits a contract" }
  virtual void f2(int i) pre (test(i)){}
  virtual int f3(int i) { return i; }
  void f4(int i){};
  virtual void f5(int i) {};

  virtual ~Child2() {}// { dg-warning "Function implicitly inherits a contract" }
};

}; //namespace T2

template<typename T>
struct Child3 :  Base<T>
{
  virtual void f1(int i) {}; // { dg-warning "Function implicitly inherits a contract" }
  virtual void f2(int i) pre (test(i)){}
  virtual int f3(int i) pre (test(i)){};
  void f4(int i){};
  virtual void f5(int i) {};

  virtual ~Child3() {}// { dg-warning "Function implicitly inherits a contract" }
};

*/

int main(int, char**)
{

  {
    T1::Base b;
    T1::Child c;
    T1::GChild gc;
    T1::GGChild gc;
  }
  {
    T2::Base b;
    T2::Child c;
    T2::GChild gc;
    T2::GGChild gc;
  }
  {
    T3::Base b;
    T3::Child c;
    T3::GChild gc;
    T3::GGChild gc;
  }


  return 0;
}
