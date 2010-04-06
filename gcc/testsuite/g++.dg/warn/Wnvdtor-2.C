// PR c++/7302
// { dg-do compile }
// { dg-options "-Wnon-virtual-dtor" }

// Warn when a class has virtual functions and accessible non-virtual
// destructor, in which case it would be possible but unsafe to delete
// an instance of a derived class through a pointer to the base class.

struct A // { dg-bogus "non-virtual destructor" }
{
protected:
  ~A();
public:
  virtual void f() = 0;
};

struct B // { dg-bogus "non-virtual destructor" }
{
private:
  ~B();
public:
  virtual void f() = 0;
};

struct C // { dg-warning "non-virtual destructor" }
{
  virtual void f() = 0;
};

struct D // { dg-warning "non-virtual destructor" }
{
  ~D();
  virtual void f() = 0;
};

struct E;

struct F // { dg-warning "non-virtual destructor" }
{
protected:
  friend class E;
  ~F();
public:
  virtual void f() = 0;
};

struct G // { dg-warning "non-virtual destructor" }
{
private:
  friend class E;
  ~G();
public:
  virtual void f() = 0;
};
