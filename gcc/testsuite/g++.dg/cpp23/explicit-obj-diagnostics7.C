// P0847R7
// { dg-do compile { target c++23 } }

// diagnose xobj member functions that override
// or are declared as virtual, override, or final

struct B {
  virtual void f0() {} // { dg-note {virtual function declared here} }
  virtual void f1() {} // { dg-note {virtual function declared here} }
  virtual void f2() {} // { dg-note {virtual function declared here} }
  virtual void f3() {} // { dg-note {virtual function declared here} }
  virtual void f4() {} // { dg-note {virtual function declared here} }
  virtual void f5() {} // { dg-note {virtual function declared here} }
  virtual void f6() {} // { dg-note {virtual function declared here} }
  virtual void f7() {} // { dg-note {virtual function declared here} }
  virtual ~B() {}
};

struct S : B {
  virtual void f0(this S&) {}		     // { dg-line line_f0 }
  virtual void f1(this S&) override {}	     // { dg-line line_f1 }
  virtual void f2(this S&) final {}	     // { dg-line line_f2 }
  virtual void f3(this S&) override final {} // { dg-line line_f3 }
  void f4(this S&) {}			     // { dg-line line_f4 }
  void f5(this S&) override {}		     // { dg-line line_f5 }
  void f6(this S&) final {}		     // { dg-line line_f6 }
  void f7(this S&) override final {}	     // { dg-line line_f7 }
};

// { dg-error {an explicit object member function cannot be 'virtual'} "" { target *-*-* } line_f0 }
// { dg-error {an explicit object member function cannot be 'virtual'} "" { target *-*-* } line_f1 }
// { dg-error {an explicit object member function cannot be 'virtual'} "" { target *-*-* } line_f2 }
// { dg-error {an explicit object member function cannot be 'virtual'} "" { target *-*-* } line_f3 }

// { dg-error {explicit object member function overrides virtual function} "" { target *-*-* } line_f0 }
// { dg-error {explicit object member function overrides virtual function} "" { target *-*-* } line_f1 }
// { dg-error {explicit object member function overrides virtual function} "" { target *-*-* } line_f2 }
// { dg-error {explicit object member function overrides virtual function} "" { target *-*-* } line_f3 }
// { dg-error {explicit object member function overrides virtual function} "" { target *-*-* } line_f4 }
// { dg-error {explicit object member function overrides virtual function} "" { target *-*-* } line_f5 }
// { dg-error {explicit object member function overrides virtual function} "" { target *-*-* } line_f6 }
// { dg-error {explicit object member function overrides virtual function} "" { target *-*-* } line_f7 }

// these should be suppressed, the wording conflicts with the error
// the issue is not that they don't override, it's that they do override, and that isn't allowed
// { dg-bogus "marked 'override', but does not override" "" { target *-*-* } line_f1 }
// { dg-bogus "marked 'final', but is not virtual"	 "" { xfail *-*-* } line_f2 }
// { dg-bogus "marked '(override|final)'"		 "" { xfail *-*-* } line_f3 }

// { dg-bogus "marked 'override', but does not override" "" { target *-*-* } line_f5 }
// { dg-bogus "marked 'final', but is not virtual"	 "" { xfail *-*-* } line_f6 }
// { dg-bogus "marked '(override|final)'"		 "" { xfail *-*-* } line_f7 }

// { dg-note "explicit object parameter declared here" "" { target *-*-* } line_f0 }
// { dg-note "explicit object parameter declared here" "" { target *-*-* } line_f1 }
// { dg-note "explicit object parameter declared here" "" { target *-*-* } line_f2 }
// { dg-note "explicit object parameter declared here" "" { target *-*-* } line_f3 }
// { dg-note "explicit object parameter declared here" "" { xfail *-*-* } line_f4 }
// { dg-note "explicit object parameter declared here" "" { xfail *-*-* } line_f5 }
// { dg-note "explicit object parameter declared here" "" { xfail *-*-* } line_f6 }
// { dg-note "explicit object parameter declared here" "" { xfail *-*-* } line_f7 }

struct S1 {
  virtual void f0(this S&) {}		     // { dg-line line_S1_f0 }
  virtual void f1(this S&) override {}	     // { dg-line line_S1_f1 }
  virtual void f2(this S&) final {}	     // { dg-line line_S1_f2 }
  virtual void f3(this S&) override final {} // { dg-line line_S1_f3 }
  void f4(this S&) {}
  void f5(this S&) override {}		     // { dg-line line_S1_f5 }
  void f6(this S&) final {}		     // { dg-line line_S1_f6 }
  void f7(this S&) override final {}	     // { dg-line line_S1_f7 }
};

// { dg-error "an explicit object member function cannot be 'virtual'" "" { target *-*-* } line_S1_f0 }
// { dg-error "an explicit object member function cannot be 'virtual'" "" { target *-*-* } line_S1_f1 }
// { dg-error "an explicit object member function cannot be 'virtual'" "" { target *-*-* } line_S1_f2 }
// { dg-error "an explicit object member function cannot be 'virtual'" "" { target *-*-* } line_S1_f3 }

// { dg-note "explicit object parameter declared here" "" { target *-*-* } line_S1_f0 }
// { dg-note "explicit object parameter declared here" "" { target *-*-* } line_S1_f1 }
// { dg-note "explicit object parameter declared here" "" { target *-*-* } line_S1_f2 }
// { dg-note "explicit object parameter declared here" "" { target *-*-* } line_S1_f3 }

// I think I want these suppressed, but theres a decent argument that they should stay
// theres arguably no reason the error about virtual should suppress these
// { dg-bogus "marked 'override', but does not override" "" { xfail *-*-* } line_S1_f1 }
// { dg-bogus "marked 'final', but is not virtual"	 "" { xfail *-*-* } line_S1_f2 }
// { dg-bogus "marked '(override|final)'"		 "" { xfail *-*-* } line_S1_f3 }

// I don't want to suppress these, there is nothing that could possibly be overridden
// even if the xobj param was removed
// { dg-error "marked 'override', but does not override" "" { target *-*-* } line_S1_f5 }
// { dg-error "marked 'final', but is not virtual"	 "" { target *-*-* } line_S1_f6 }
// { dg-error "marked '(override|final)'"		 "" { target *-*-* } line_S1_f7 }

