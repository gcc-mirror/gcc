// PR c++/13663

struct S {
  void f();
};

void g(int);
void g(double);

void h () {
  S s;
  for (;;s.f); // { dg-error "" }
  for (;;g); // { dg-error "" }
}
