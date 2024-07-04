/* Simple test of -gprune-btf option operation.
   Since 'struct foo' is not used, no BTF shall be emitted for it.  */

/* { dg-do compile } */
/* { dg-options "-gbtf -gprune-btf -dA" } */

/* No BTF info for 'struct foo' nor types used only by it.  */
/* { dg-final { scan-assembler-not "BTF_KIND_STRUCT 'foo'" } } */
/* { dg-final { scan-assembler-not "BTF_KIND_INT 'char'" } } */

/* We should get BTF info for 'struct bar' since it is used.  */
/* { dg-final { scan-assembler "BTF_KIND_STRUCT 'bar'"} } */

struct foo {
  int a;
  char c;
};

struct bar {
  int x;
  long z[4];
};

struct bar a_bar;

