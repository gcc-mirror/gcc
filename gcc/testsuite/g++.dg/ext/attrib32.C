// PR c++/35315

typedef union { int i; } U __attribute__((transparent_union));

static void foo(U) {}
static void foo(int) {}

void bar()
{
  foo(0);
}
