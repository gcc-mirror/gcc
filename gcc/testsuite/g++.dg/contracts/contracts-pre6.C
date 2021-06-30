// ensure no errors are thrown when we have to insert a decl for the internal
// unchecked function after leaving a (possibly nested) namespace
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

namespace ns0
{
  int f(int a) [[ pre: a > 0 ]];
}

int ns0::f(int a) [[ pre: a > 0 ]]
{
  return -a;
}

namespace ns0
{
  namespace ns1
  {
    int f(int a) [[ pre: a > 0 ]];
  }
}

int ns0::ns1::f(int a) [[ pre: a > 0 ]]
{
  return -a;
}

namespace ns0
{
  namespace ns1
  {
    int f2(int a) [[ pre: a > 0 ]];
    namespace ns2
    {
      int f(int a) [[ pre: a > 0 ]];
    }
  }
  int ns1::f2(int a) [[ pre: a > 0 ]]
  {
    return -a;
  }
}

int ns0::ns1::ns2::f(int a) [[ pre: a > 0 ]]
{
  return -a;
}

namespace ns0
{
  struct S
  {
    int f(int a) [[ pre: a > 0 ]];
  };
  namespace ns1
  {
    struct S2
    {
      int f(int a) [[ pre: a > 0 ]];
    };
  }
}

int ns0::S::f(int a) [[ pre: a > 0 ]]
{
  return -a;
}

int ns0::ns1::S2::f(int a) [[ pre: a > 0 ]]
{
  return -a;
}

