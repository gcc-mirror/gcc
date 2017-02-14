// { dg-do compile { target c++11 } }
// { dg-prune-output "overriding" }

struct A
{
  virtual void f();
  virtual void g() throw();
  virtual void h() noexcept;
  virtual void i() noexcept(false);
  virtual void j() throw(int);	// { dg-error "dynamic exception specification" "" { target c++1z } }
};				// { dg-warning "deprecated" "" { target { ! c++1z } } .-1 }

struct B: A
{
  void f() noexcept;
  void g() noexcept;
  void h() noexcept;
  void i() noexcept;
  void j() noexcept;
};

struct C: A
{
  void f() throw();
  void g() throw();
  void h() throw();
  void i() throw();
  void j() throw();
};

struct D: A
{
  void f() noexcept(false);
  void g() noexcept(false);	// { dg-error "looser" }
  void h() noexcept(false);	// { dg-error "looser" }
  void i() noexcept(false);
  void j() noexcept(false);	// { dg-error "looser" "" { target { ! c++1z } } }
};

struct E: A
{
  void f() throw(int);		// { dg-error "dynamic exception specification" "" { target c++1z } }
				// { dg-warning "deprecated" "" { target { ! c++1z } } .-1 }
  void g() throw(int);		// { dg-error "looser" }
				// { dg-error "dynamic exception specification" "" { target c++1z } .-1 }
				// { dg-warning "deprecated" "" { target { ! c++1z } } .-2 }
  void h() throw(int);		// { dg-error "looser" }
				// { dg-error "dynamic exception specification" "" { target c++1z } .-1 }
				// { dg-warning "deprecated" "" { target { ! c++1z } } .-2 }
  void i() throw(int);		// { dg-error "dynamic exception specification" "" { target c++1z } }
				// { dg-warning "deprecated" "" { target { ! c++1z } } .-1 }
  void j() throw(int);		// { dg-error "dynamic exception specification" "" { target c++1z } }
				// { dg-warning "deprecated" "" { target { ! c++1z } } .-1 }
};

struct F: A
{
  void f();
  void g();			// { dg-error "looser" }
  void h();			// { dg-error "looser" }
  void i();
  void j();			// { dg-error "looser" "" { target { ! c++1z } } }
};
