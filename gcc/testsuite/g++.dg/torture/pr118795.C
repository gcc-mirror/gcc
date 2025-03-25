// { dg-do compile }

unsigned char *a();
struct b {
  void c() const;
};
void b::c() const {
  unsigned char *d = a(), *e = a();
  for (long f; f; ++f) {
    e[0] = e[1] = e[2] = d[0];
    e[3] = d[0];
    d += 4;
    e += 4;
  }
}
