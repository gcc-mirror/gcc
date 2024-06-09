// PR c++/113987
// { dg-do compile }
// { dg-options "-Wuninitialized" }

struct t1 {
    t1(int);
};
struct t2 {
    t2(int&, int = 0);
    t2(double&, int = 0);
};
struct t3 {
    t3(int&);
};
struct t4 {};
void f1(int&);
struct t {
    t() :
      v1(i),  // { dg-warning "is used uninitialized" }
      v2(i),
      v3(i),
      v4((f1(i), t4())),
      v5(i) {}
    t1 v1;
    t2 v2;
    t3 v3;
    t4 v4;
    t1 v5;
    int i;
    int j;
};
int main() { t v1; }
