// { dg-do compile }
// { dg-additional-options "-Wno-psabi" }

#define vector __attribute__((vector_size(16)))
class A {
  friend vector float f();
  __attribute__((deprecated)) friend void f2();
  friend __attribute__((deprecated, vector_size(16))) float f3();
  friend __attribute__((vector_size(16), deprecated)) float f4();
};

vector float vf;
vector float
f ()
{
  return vf;
}

void
f2 ()
{
}

vector float
f3 ()
{
  return vf;
}

vector float
f4 ()
{
  return vf;
}
