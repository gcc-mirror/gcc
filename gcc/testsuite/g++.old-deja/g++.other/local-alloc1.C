// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>
// Special g++ Options: -O0 -fpic
// Skip if target: cris-*-elf* cris-*-aout* mmix-*-*

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
