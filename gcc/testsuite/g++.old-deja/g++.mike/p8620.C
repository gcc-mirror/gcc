// Build don't link:
// prms-id: 8620

struct S {
  long l;
};

typedef unsigned long l;

void f() {
  S* p;
  if (p->l < 0)
    return;
}
