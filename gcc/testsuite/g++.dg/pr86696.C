/* PR tree-optimization/86696 - ICE in handle_char_store at
   gcc/tree-ssa-strlen.c
   { dg-do compile }
   { dg-options "-O2 -Wall -std=c++11" } */

typedef char a;
template <typename b> struct c {
  int d;
  b e;
};
struct f;
class g {
public:
  void h(c<f>);
};
enum i {};
enum j : a { k, l };
struct f {
  i m;
  a n;
  a o;
  a p;
  j family;
};
void fn1() {
  i format{};
  f info{format, a(), 0, 4, l};
  g dest;
  dest.h({format, info});
}
