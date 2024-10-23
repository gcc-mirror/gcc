/* { dg-do compile } */
/* { dg-options "-O1" } */

struct a {
  int b;
};
void c(int, int);
void __attribute__((returns_twice))
bar1(struct a);
void bar(struct a) {
  struct a d;
  bar1(d);
  c(d.b, d.b);
}
