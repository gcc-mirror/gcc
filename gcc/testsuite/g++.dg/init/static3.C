// { dg-do run }

struct T
{
  static void (*handler)();
  static void func() {};
};

bool fail;

struct S {
  S() {
    if (T::handler != T::func)
      fail = true;
  }
};

static S s;

void (*T::handler)() = func;

int main()
{
  if (fail)
    return 1;
}
