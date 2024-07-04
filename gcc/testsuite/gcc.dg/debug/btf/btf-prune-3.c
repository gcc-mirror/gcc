/* Test that -gprune-btf does not prune through array members.  */

/* { dg-do compile } */
/* { dg-options "-gbtf -gprune-btf -dA" } */

/* We expect full BTF information each struct.  */
/* { dg-final { scan-assembler "TYPE \[0-9\]+ BTF_KIND_FWD 'file'" } } */
/* { dg-final { scan-assembler "TYPE \[0-9\]+ BTF_KIND_STRUCT 'A'" } } */
/* { dg-final { scan-assembler "TYPE \[0-9\]+ BTF_KIND_STRUCT 'B'" } } */
/* { dg-final { scan-assembler "TYPE \[0-9\]+ BTF_KIND_STRUCT 'C'" } } */

struct file;

struct A {
  void *private;
  long (*read)(struct file *, char *, unsigned long);
  long (*write)(struct file *, const char *, unsigned long);
};

struct B {
  unsigned int x;
  struct A **as;
};

struct C {
  struct A *arr_a[4];
  struct A *lone_a;
  unsigned int z;
};

unsigned int
foo (struct B *b, struct C *c)
{
  return b->x + c->z;
}
