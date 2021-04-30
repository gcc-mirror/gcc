// { dg-do compile }

#define vector __attribute__((vector_size(16)))
class A {
  friend vector float f();
  __attribute__((deprecated)) friend void f2(); // { dg-error "attribute appertains" }
  friend __attribute__((deprecated, vector_size(16))) float f3(); // { dg-error "attribute appertains" }
  friend __attribute__((vector_size(16), deprecated)) float f4(); // { dg-error "attribute appertains" }
};

vector float vf;
vector float
f ()
{
  return vf;
}
