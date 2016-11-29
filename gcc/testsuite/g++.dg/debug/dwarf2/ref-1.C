// { dg-do compile }
// { dg-options "-O -g -dA -gno-strict-dwarf" }
// { dg-final { scan-assembler-not "DW_TAG_const_type" { xfail { powerpc-ibm-aix* } } } }

int x;
int &y = x;

typedef int &z_t;
z_t z = x;

void f(int &p) {}

struct foo {
  int &bar;
  typedef int &bart;
  bart fool;
};

void f3(struct foo &p) {}
