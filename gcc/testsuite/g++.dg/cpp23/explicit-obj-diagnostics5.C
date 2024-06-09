// P0847R7
// { dg-do compile { target c++23 } }

// rejection and diagnosis of invalid uses of 'this' in body of xobj member functions

// { dg-message "In explicit object member function" "" { target *-*-* } 0 }

struct S0 {
  int _n;
  void f(this S0& s) { // { dg-note {use explicit object parameter 's' instead} } 
    this->_n = 10; // { dg-error "'this' is unavailable for explicit object member functions" }
    // suppress unused variable warning
    static_cast<void>(s);
  }
};

struct S1 {
  int _n;
  void f(this S1&) { // { dg-note "name the explicit object parameter" }
    this->_n = 10; // { dg-error "'this' is unavailable for explicit object member functions" }
  }
};

