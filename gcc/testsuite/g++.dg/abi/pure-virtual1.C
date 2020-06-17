// Test that we don't need libsupc++ just for __cxa_pure_virtual.
// { dg-do link }
// { dg-require-weak }
// { dg-additional-options "-fno-rtti -nodefaultlibs -lc" }
// { dg-xfail-if "AIX weak" { powerpc-ibm-aix* } }

struct A
{
  int i;
  virtual void f() = 0;
  A(): i(0) {}
};

struct B: A
{
  virtual void f() {}
};

int main()
{
  B b;
}
