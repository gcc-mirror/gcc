// PR c++/101182
// { dg-do compile { target concepts } }

int a;
void g(bool);

bool f() {
  g(requires { a++; });
  return requires { a++; };
}
