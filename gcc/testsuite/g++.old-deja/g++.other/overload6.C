// { dg-do run  }
extern "C" void abort();

struct S1
{
  int f() { return 0; }
  int f() const { return 1; }
};

struct S2 : public S1
{
};

int main()
{
  S2 s2;
  if (s2.f() != 0)
    abort ();
}
