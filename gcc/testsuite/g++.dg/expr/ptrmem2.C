struct S {
  int i;
};

int S::**p;
int S::*q;

void f() {
  p[1] = q;
}


