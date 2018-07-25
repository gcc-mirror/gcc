// { dg-do compile }
// { dg-options "-pedantic" }

class foo {
public:
  void operator& (int = 1);  // { dg-error "8:.void foo::operator&\\(int\\). cannot have default arguments" }
  void operator++ (int = 2); // { dg-error "8:.void foo::operator\\+\\+\\(int\\). cannot have default arguments" }
  void operator-- (int = 3); // { dg-error "8:.void foo::operator--\\(int\\). cannot have default arguments" }
};
