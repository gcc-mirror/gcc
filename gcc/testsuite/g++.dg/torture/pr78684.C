// PR middle-end/78684
// { dg-do compile }

class a {
public:
  a(long);
  void operator<<=(long) {
    long b;
    for (unsigned long c; c; c--)
      d[c + b] = d[c];
  }
  a &g();
  long d[28];
};
long e;
int f;
void j() {
  a h(e), i = h;
  i.g() <<= f;
}
