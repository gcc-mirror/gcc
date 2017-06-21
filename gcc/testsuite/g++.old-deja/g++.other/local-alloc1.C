// { dg-do assemble { target fpic } }
// { dg-options "-O0 -fpic" }
// { dg-skip-if "requires unsupported run-time relocation" { spu-*-* } }
// Origin: Jakub Jelinek <jakub@redhat.com>

struct bar {
  bar() {}
  double x[3];
};

static bar y[4];

void foo(int z)
{
  bar w;
  y[z] = w;
}
