// PR middle-end/15054

// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort (void);

void
__attribute__((noinline))
check (long x, long y)
{
  if (x != y)
    abort ();
}

struct A
{
  A() : a(2) { check (a, 2); }
  ~A() { check (a, 2); }
private:
  long a;
};

class B {
  long b;
  B& operator =(const B& );
public:
  B (long p) : b(p) { check (b, 6); }
  B (const B& p) : b(p.b) { check (b, 6); }
  ~B () { check (b, 6); A obj; check (b, 6); }
  B foo() { return B(*this); }
};

int main ()
{
  B o(6);
  o.foo().foo();
  return 0;
}
