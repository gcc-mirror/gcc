// PR tree-optimization/78060
// { dg-do compile }
// { dg-options "-O3 -fsplit-loops" }
class A {
public:
  template <typename T2> int &operator[](T2);
};
int a;
A b;
void fn1() {
  long c;
  for (int l; l < c; ++l)
    b[l] = l < 2 ?: a;
}
