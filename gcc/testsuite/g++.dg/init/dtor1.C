// { dg-do run }

extern "C" void abort ();

int d = 2;

struct B
{
  int x;
  B (int i) : x (i) { }
  ~B () { if (d-- != x) abort (); }
};

struct A
  : public B
{
  A () : B (0), x1(1), x2(2) {}
  B x1;
  B x2;
};


int main ()
{
  A a;
  return 0;
}
