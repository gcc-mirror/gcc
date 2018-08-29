// { dg-do run }

// Test that A's copy assignment method is called when B's instance
// member array of A is assigned.

// Contributed by Brian Gaeke, public domain.
int status = 1;

class A
{
public:
  int i;
  A &operator =(const A &i);
};

A a;

A& A::operator=(const A &i)  {
    status = 0;
    return a;
  }

class B
{
public:
  A arr[10];
};

int main (int argc, char **argv)
{
  B b;
  b.arr[0].i = 15;
  B a;
  a = b; // trigger copy assignment
  return status;
}
