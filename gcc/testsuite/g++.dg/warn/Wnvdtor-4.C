// { dg-do compile }
// { dg-options "-Weffc++ -Wno-non-virtual-dtor" }

// Warn when a class has virtual functions and accessible non-virtual
// destructor, in which case it would be possible but unsafe to delete
// an instance of a derived class through a pointer to the base class.

struct A
{
protected:
  ~A();
public:
  virtual void f() = 0;
};

struct B
{
private:
  ~B();
public:
  virtual void f() = 0;
};

struct C
{
  virtual void f() = 0;
};

struct D
{
  ~D();
  virtual void f() = 0;
};

struct E;

struct F
{
protected:
  friend class E;
  ~F();
public:
  virtual void f() = 0;
};

struct G
{
private:
  friend class E;
  ~G();
public:
  virtual void f() = 0;
};

struct H {};

struct I1 : H
{};
struct I2 : private H
{};

struct J1 : H
{ virtual ~J1 ();};
struct J2 : private H
{ virtual ~J2 ();};

struct K 
{
  virtual void k ();
};

struct L1 : K
{virtual ~L1 ();};
struct L2 : private K
{virtual ~L2 ();};
