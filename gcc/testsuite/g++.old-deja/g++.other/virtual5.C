// Build don't link:
// Special g++ Options: -w
// Origin: Derived from code by Andris Pavenis <andris@stargate.astr.lu.lv>

class vector {};

class V
{
};

class A : public vector
{
};

class B : public A, virtual V, public vector
{
  B() {}
};


