// { dg-do run  }
// Test that we clean up temporaries bound to references properly when
// jumping out of their scope.

int ret = 1;

struct A
{
  ~A() { ret = 0; }
};

void f()
{
  if (0)
    {
    out:
      return;
    }
  const A& a = A();
  goto out;
}

int main()
{
  f();
  return ret;
}
