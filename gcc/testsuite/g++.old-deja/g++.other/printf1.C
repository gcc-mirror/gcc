// { dg-do assemble  }
// { dg-options "-Wall" }

struct a
{
  void x(char *f,...) __attribute__((format(printf,2,3)));
};

int main()
{
  a A;
  A.x("%d"); // { dg-warning "" } too few arguments for format
  return 0;
}
