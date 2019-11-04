// { dg-do compile { target alpha*-*-* } }
// { dg-options "-O1 -g -fno-var-tracking -mcpu=ev4 -mieee" }

struct a {
  a(long);
};
long b;
void c() {
  a d(1);
  double e = b;
  for (; b;)
    d = e;
}
