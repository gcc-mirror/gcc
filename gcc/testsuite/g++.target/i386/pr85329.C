/* { dg-do compile } */
/* { dg-require-ifunc "" } */

struct a
{
  __attribute__((target_clones("sse", "default"))) void operator^=(a) {}
} * b;

class c {
public:
  a *d();
};

class f {
  void g();
  c e;
};

void f::g() { *e.d() ^= b[0]; }
