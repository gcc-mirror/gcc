/* Default to BTF debug info.  */
/* { dg-do compile } */
/* { dg-options "-g -dA" }*/

struct A {
  int x;
  int y;
};

int
foo (struct A *a)
{
  return a->x;
}

/* { dg-final { scan-assembler-not "DWARF version" } } */
/* { dg-final { scan-assembler "btf_version" } } */
