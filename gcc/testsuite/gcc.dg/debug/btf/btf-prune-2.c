/* Test that -gprune-btf does not chase pointer-to-struct members.  */

/* { dg-do compile } */
/* { dg-options "-gbtf -gprune-btf -dA" } */

/* Only use of B is via a pointer member of C.
   Full BTF for B is replaced with a forward.  */
/* { dg-final { scan-assembler-not "BTF_KIND_STRUCT 'B'" } } */
/* { dg-final { scan-assembler-times "TYPE \[0-9\]+ BTF_KIND_FWD 'B'" 1 } } */

/* Detailed info for B is omitted, and A is otherwise unused.  */
/* { dg-final { scan-assembler-not "BTF_KIND_\[A-Z\]+ 'A'" } } */

/* { dg-final { scan-assembler "BTF_KIND_STRUCT 'C'" } } */

struct A;

struct B {
  int x;
  int (*do_A_thing) (int, int);
  struct A *other;
};

struct C {
  unsigned int x;
  struct B * a;
};

int
foo (struct C *c)
{
  return c->x;
}
