// { dg-do compile }
// { dg-options "-fvtable-gc" }
// Origin: Hans-Peter Nilsson <hp@bitrange.com>

class Base0
{
public:
  Base0(); virtual ~Base0();
  virtual void f1();
  virtual void f2();
private:
  int a_value;
};

class Base1 : public Base0
{
public:
  Base1(); virtual ~Base1();
  virtual void f1(), f2();
  virtual void f3();
};

class Base2 : public Base1
{
public:
  Base2(); virtual ~Base2();
  virtual void f1(), f2();
  virtual void f4();
};

class VbasedA : virtual public Base2
{
public:
  VbasedA(); virtual ~VbasedA();
  virtual void f1(), f2(), f3();
  virtual void f6();
};

class Side0
{
public:
  Side0(); virtual ~Side0();
  virtual void x1();
  virtual void xx();
private:
  int ryan;
};

class Multisv0 : public Side0, virtual public Base2
{
public:
  Multisv0(); virtual ~Multisv0();
  virtual void f1(), f2();
  virtual void f3();
  virtual void f6();
  virtual void xx();
};

class Multivs1 : public Base2, virtual public Side0
{
public:
  Multivs1(); virtual ~Multivs1(); virtual void f1(); virtual void fx2();
  virtual void fx4(), fx5();
  virtual void f6();
  virtual void xx();
};

class Multiss2 : public Base2, public Side0
{
public:
  Multiss2(); virtual ~Multiss2(); virtual void f1(); virtual void fx2();
  virtual void fx4();
  virtual void f6();
  virtual void xx();
};

class Multivv3 : virtual public Base2, virtual public Side0
{
public:
  Multivv3(); virtual ~Multivv3(); virtual void f1(); virtual void fx2();
  virtual void fx4(), fx5();
  virtual void f6();
  virtual void xx();
};

Base0::Base0() {}
Base0::~Base0() {}
Base1::Base1() {}
Base1::~Base1() {}
Base2::Base2() {}
Base2::~Base2() {}
VbasedA::VbasedA() {}
VbasedA::~VbasedA() {}
Multisv0::Multisv0() {}
Multisv0::~Multisv0() {}
Multivs1::Multivs1() {}
Multivs1::~Multivs1() {}
Multiss2::Multiss2() {}
Multiss2::~Multiss2() {}
Multivv3::Multivv3() {}
Multivv3::~Multivv3() {}
Side0::Side0() {}
Side0::~Side0() {}

extern void x (VbasedA *);
extern void x2 (Multisv0 *);
extern void x3 (Multivs1 *);
extern void x4 (Multiss2 *);
extern void x5 (Multivv3 *);
void y () { VbasedA ii; x(&ii);}
void y2 () { Multisv0 ii; x2(&ii);}
void y3 () { Multivs1 ii; x3(&ii);}
void y4 () { Multiss2 ii; x4(&ii);}
void y5 () { Multivv3 ii; x5(&ii);}
void x (VbasedA *ii) { ii->f2();}
void x2 (Multisv0 *ii) { ii->f2();}
void x3 (Multivs1 *ii) { ii->f2();}
void x4 (Multiss2 *ii) { ii->f2();}
void x5 (Multivv3 *ii) { ii->f2();}

// Use .* because of ia64's convention of marking symbols with "#", which
// makes it through the c++filt.

// { dg-final { scan-assembler-dem "\.vtable_inherit\[ \t\]*vtable for Multivv3.*0" } }
// { dg-final { scan-assembler-dem "\.vtable_inherit\[ \t\]*VTT for Multivv3.*0" } }
// { dg-final { scan-assembler-dem "\.vtable_inherit\[ \t\]*vtable for Multiss2.*vtable for Base2" } }
// { dg-final { scan-assembler-dem "\.vtable_inherit\[ \t\]*vtable for Multivs1.*vtable for Base2" } }
// { dg-final { scan-assembler-dem "\.vtable_inherit\[ \t\]*VTT for Multivs1.*vtable for Base2" } }
// { dg-final { scan-assembler-dem "\.vtable_inherit\[ \t\]*vtable for Multisv0.*vtable for Side0" } }
// { dg-final { scan-assembler-dem "\.vtable_inherit\[ \t\]*VTT for Multisv0.*vtable for Side0" } }
// { dg-final { scan-assembler-dem "\.vtable_inherit\[ \t\]*vtable for Side0.*0" } }
// { dg-final { scan-assembler-dem "\.vtable_inherit\[ \t\]*vtable for VbasedA.*0" } }
// { dg-final { scan-assembler-dem "\.vtable_inherit\[ \t\]*VTT for VbasedA.*0" } }
// { dg-final { scan-assembler-dem "\.vtable_inherit\[ \t\]*vtable for Base2.*vtable for Base1" } }
// { dg-final { scan-assembler-dem "\.vtable_inherit\[ \t\]*vtable for Base1.*vtable for Base0" } }
// { dg-final { scan-assembler-dem "\.vtable_inherit\[ \t\]*vtable for Base0.*0" } }
