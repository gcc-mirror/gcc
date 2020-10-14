/* { dg-options "-mno-strict-align" } */
/* { dg-do compile } */

struct S {
  int a;
  double b;
};
struct S GetNumbers();
struct S g;

void foo(){
  g = GetNumbers();
}
