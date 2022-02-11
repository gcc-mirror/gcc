// REQUIRED_ARGS: -O
void t()
{
  auto a = A(false ? B().p : null);
}

struct A
{
  void* p;
}

struct B
{
  void* p;
  ~this() {}
}
