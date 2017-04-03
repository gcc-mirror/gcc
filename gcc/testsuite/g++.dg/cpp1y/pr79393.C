// { dg-do compile { target c++14 } }
// PR c++/79393 deduced eh spec, deleted dtors and vbases

struct A3;

struct VDT {
  virtual ~VDT () noexcept (false);
};

struct A1 : virtual VDT {
  virtual void abstract () = 0;
};

struct A2 : A1 {  };

struct A3 : A2 
{
  virtual void abstract ();
};

A3 a3;
