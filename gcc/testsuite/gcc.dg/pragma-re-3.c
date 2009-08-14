
#pragma redefine_extname f1 f
#pragma redefine_extname g1 g

void f() {
  extern int f1();
  f1();
}

void g() {
  g1();
}

int main () {
  f();
  g();
}
