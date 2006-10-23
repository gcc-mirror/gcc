// { dg-do compile { target s390x-*-* } }
// { dg-options "-O3 -fPIC" }

class A
{
public:
  void f (void) { _M_a = 0; }
  void g (void) { _M_a = 1; }
  void h (void);

private:
  int _M_a;
};

class B : virtual public A
{
};

void
test (B& x)
{
  for (int i = 0; i < 17; i++)
   {
     x.f ();
     (x.*&A::g) ();
     x.h ();
   }
}

// Check that every call to A::g goes via the PLT.
// { dg-final { scan-assembler-not "brasl\[^@\]*\n" } }

