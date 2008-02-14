// PR5645: gcc warns that pure virtual class not explicitly initialized.  
// { dg-do compile }
// { dg-options "-Wall -Wextra" }

class a {
public:
  virtual int f() = 0;
  virtual int g() = 0;
};

class b : public a {
public:
  b();
  b(const b& c);

protected:
  int i;
};

b::b() {}

b::b(const b& c) { // { dg-bogus "base class .class a. should be explicitly initialized in the copy constructor" }
  i = c.i;
}

struct X {};

struct Y : X
{
  Y (Y const&) {}
};

