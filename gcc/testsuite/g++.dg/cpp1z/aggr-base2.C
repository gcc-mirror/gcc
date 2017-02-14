// { dg-options -std=c++1z }

struct derived;
struct base {
  friend struct derived;
private: 
  base();
};
struct derived : base {};

derived d1{};			// { dg-error "" "" { target c++1z } }
derived d2;			// still OK
