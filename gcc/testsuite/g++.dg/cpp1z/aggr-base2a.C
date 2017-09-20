// { dg-do compile { target c++11 } }

struct derived;
struct base {
  friend struct derived;
private: 
  base();
};
struct derived : base {};

derived d1{};			// { dg-error "" "" { target c++17 } }
derived d2;			// still OK
