// Build don't run:
struct foo
{
  foo() {};
  ~foo() {};
  void func() { static foo x; };
};

int main()
{
   foo f;
   return 0;
}
