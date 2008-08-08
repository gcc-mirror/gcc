// { dg-do compile } 
// { dg-options "-Wuninitialized" }

class X {
  int & flag;// { dg-warning "non-static reference 'int& X::flag' in class without a constructor" }
public:
  void f(){ flag++ ; }
};

class Y {
  const int var;// { dg-warning "non-static const member 'const int Y::var' in class without a constructor" }
public:
  int g(){ return 2*var; }
};
