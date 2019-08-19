/* { dg-do compile } */

struct S {
  ~S();
};
int a = 123;
void fn1() {
  S *s = new S[a];
  delete[] s;
}
