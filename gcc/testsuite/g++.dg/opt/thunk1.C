// PR 6788
// Test that the thunk adjusts the this pointer properly.
// { dg-do run }
// { dg-require-effective-target size20plus }

extern "C" void abort ();

struct A
{
  virtual void foo() = 0;
  char large[33*1024U];
};

struct B
{
  virtual void foo() = 0;
};

struct C : public A, public B
{
  virtual void foo();
};

static C *match;

void C::foo()
{
  if (this != match)
    abort ();
}

void bar(B *x)
{
  x->foo();
}

int main()
{
  C obj;
  match = &obj;
  bar(&obj);
  return 0;
}
