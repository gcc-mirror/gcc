// Test that we can have an unnamed struct inside an anonymous union.

struct A
{
  union
  {
    struct { int i; } foo;
  };
};

static union
{
  struct { int i; } foo;
};

int main ()
{
  union
  {
    struct { int i; } bar;
  };
}
