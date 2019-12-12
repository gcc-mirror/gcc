// PR c++/84059
// { dg-do compile }
// { dg-require-ifunc "" }

template <typename> struct a
{
  int __attribute__ ((target ("arch=ivybridge"))) c (int) {return 1;}
  int __attribute__ ((target ("default"))) c (int) { return 2; }
};
void
d ()
{
  a<double> b;
  b.c (2);
}
