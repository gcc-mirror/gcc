// { dg-options "-std=c++0x" }
// { dg-prune-output "overriding" }

struct A
{
  virtual void f();
  virtual void g() throw();
  virtual void h() noexcept;
  virtual void i() noexcept(false);
  virtual void j() throw(int);
};

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
  void j() noexcept(false);	// compatible; treated as throw(int)
};

struct E: A
{
  void f() throw(int);
  void g() throw(int);		// { dg-error "looser" }
  void h() throw(int);		// { dg-error "looser" }
  void i() throw(int);
  void j() throw(int);
};

struct F: A
{
  void f();
  void g();			// { dg-error "looser" }
  void h();			// { dg-error "looser" }
  void i();
  void j();			// { dg-error "looser" }
};
