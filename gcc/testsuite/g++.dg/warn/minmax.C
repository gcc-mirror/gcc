int i, j, k;

void f() {
  i = j <? k; // { dg-warning "deprecated" }
  i = j >? k; // { dg-warning "deprecated" }
  i <?= j; // { dg-warning "deprecated" }
  i >?= j; // { dg-warning "deprecated" }
}

struct S {
  void operator<?(int); // { dg-warning "deprecated" }
  void operator>?(int); // { dg-warning "deprecated" }
  void operator<?=(int); // { dg-warning "deprecated" }
  void operator>?=(int); // { dg-warning "deprecated" }
};
