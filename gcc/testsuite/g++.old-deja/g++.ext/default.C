// { dg-do assemble  }
// PRMS Id: 5353
// This may be an extension, but it's a very common one...

extern "C" int printf (const char *, ...);

class A {
public:
  static A*func (int = 3); 
  static A*(*ptr)(int = 4); // { dg-error "" } .*
};

A*(*A::ptr)(int) = &A::func;

int main()
{
  A foo;

  A::ptr(); // { dg-error "" } .*
  A::ptr(47);
}

A*A::func(int i)
{
  printf("I = %d\n",i);
  return (A*)0;
}
