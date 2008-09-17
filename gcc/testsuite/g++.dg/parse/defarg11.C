// { dg-do compile }
// { dg-options "-pedantic" }

class foo {
public:
  void operator& (int = 1);  // { dg-error "default argument" }
  void operator++ (int = 2); // { dg-warning "default argument" }
  void operator-- (int = 3); // { dg-warning "default argument" }
};
