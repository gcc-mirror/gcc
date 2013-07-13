// { dg-do run }
// { dg-require-effective-target c++11 }

struct A
{
  int i;
  A() = default;
  A(int i): i{i} { }
  ~A() {}
};

int main(int argc, char **argv)
{
  { int i[4] = { 42, 42, 42, 42 }; }
  {
    A a[4] = { argc };
    if (a[1].i != 0)
      __builtin_abort ();
  }
}
