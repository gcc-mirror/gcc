// { dg-do assemble  }

struct A {
  int operator[] (int);
};

//int A::operator[] (int);

int A::operator[] (int j)
{
  return j * j;
}

extern A a;

int q () { return a[99]; }
