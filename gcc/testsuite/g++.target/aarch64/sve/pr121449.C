/* PR target/121449 */
/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 -save-temps" } */

struct example;

struct array {
  unsigned length();
  example *operator[](unsigned i) {
    example **data = reinterpret_cast<example **>(this);
    return data[i];
  }
};

struct example {
  int a[16];
  bool is_even;
  int version;
  int count() { return is_even ? 2 : 1; }
  void fun1(int, long);
  void fun2(unsigned, unsigned);
  void process(array &, array &);
};

bool found;

void example::process(array &a, array &b) {
  for (unsigned i = 1; a.length(); i++) {
    long total = 0;
    for (unsigned k = 0; k <= i; k++) {
      total += a[k]->count();
    }
    for (unsigned j = 0; j < i; j++) {
      int major = b[j]->version;
      if (found)
        major += i;
      fun1(i + 1, total);
      fun2(j, major);
    }
  }
}

/* { dg-final { scan-assembler-not {\tld1b\t(z[0-9]+)\.d, p[0-7]/z, \[(z[0-9]+)\.d, #64\]} } } */

