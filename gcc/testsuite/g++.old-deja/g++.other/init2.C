// Special g++ Options: -O3

typedef int (*fp)();

struct S
{
  fp f;
};

static int f()
{
  return 0;
}

static const S s = { &f };

int main()
{
  return (*s.f)();
}
