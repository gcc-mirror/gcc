// Test disabling
// { dg-do compile } 
// { dg-options "-Wall -Wextra -Wno-uninitialized" }

class X {
  int & flag;// { dg-bogus "non-static reference 'int& X::flag' in class without a constructor" }
public:
  void f(){ flag++ ; }
};

class Y {
  const int var;// { dg-bogus "non-static const member 'const int Y::var' in class without a constructor" }
public:
  int g(){ return 2*var; }
};
