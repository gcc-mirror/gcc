// PR c++/3948
// Test that the destructor call for a value parameter gets the
// right address.

// { dg-do run }

void *p[2];
int i;
int r;

struct C
{
  int m;
  
  C() { p[i++] = this; }
  ~C() { if (p[--i] != this) r = 1; }
};


void Foo (C c)
{
  p[i++] = &c;
}

int main ()
{       
  C c;

  Foo (c);
  return r;
}
