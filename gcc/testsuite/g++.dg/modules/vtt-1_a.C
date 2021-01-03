// { dg-additional-options "-fmodules-ts -O2 -fno-inline" }

export module foo;
// { dg-module-cmi foo }

export struct base 
{
  base () {}
  virtual ~base ();
  int m;
};

base::~base ()
{
}

export struct derived : virtual base 
{
  derived () {}
  virtual ~derived ();
  int m2;
};

derived::~derived ()
{
}

export void make_foo ()
{
  base b;
  derived d;
}

// { dg-final {scan-assembler "_ZTV4base:" } }
// { dg-final {scan-assembler "_ZTV7derived:" } }
// { dg-final {scan-assembler "_ZTT7derived:" } }
