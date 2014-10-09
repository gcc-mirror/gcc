// { dg-do compile }
// { dg-options "-Wshadow" }
// PR c++/57709 
class C {
  int both_var; // { dg-message "declaration" }
  void var_and_method(void) {} // { dg-message "declaration" }
  void m() { 
    int 
      both_var,  // { dg-warning "shadows" }
      var_and_method; 
  }
  void m2() { 
    void (C::*var_and_method)(void); // { dg-warning "shadows" }
  }
};
