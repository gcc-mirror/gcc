// { dg-do assemble  }
// { dg-options "-O0 -fpic" }
// Origin: Jakub Jelinek <jakub@redhat.com>
// { dg-skip-if "No -fpic" { cris-*-elf* cris-*-aout* mmix-*-* } { "*" } { "" } }

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
