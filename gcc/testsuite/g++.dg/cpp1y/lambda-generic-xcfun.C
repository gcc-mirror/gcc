// Explicit generic lambda conversion to function ptr test from N3690 5.1.2.6
// { dg-do compile { target c++14 } }
// { dg-options "" }

void f1(int (*)(int)) { }
void f2(char (*)(int)) { }
void g(int (*)(int)) { } // #1
void g(char (*)(char)) { } // #2
void h(int (*)(int)) { } // #3
void h(char (*)(int)) { } // #4

int main()
{
  auto glambda = [] <typename T> (T a) { return a; };
  int (*fp)(int) = glambda;
  f1(glambda); // OK
  f2(glambda); // { dg-error "invalid user-defined conversion" }
  g(glambda); // { dg-error "ambiguous" }
  h(glambda); // OK: calls #3 since it is convertible from ID
  int& (*fpi)(int*) = [] <typename T> (T* a) -> auto& { return *a; }; // OK

  auto GL = [] <typename T> (T a) { return a; };
  int (*GL_int)(int) = GL; // OK: through conversion function template
  GL_int(3);
}

