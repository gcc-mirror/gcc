/* -g defaults to BTF, which in turn implies -mco-re.  */
/* { dg-do compile } */
/* { dg-options "-g -dA" }*/

struct A {
  int x;
  int y;
};

int
foo (struct A *a)
{
  return __builtin_preserve_access_index (a->x);
}

/* { dg-final { scan-assembler-not "DWARF version" } } */
/* { dg-final { scan-assembler "btf_version" } } */
/* { dg-final { scan-assembler "btfext_version" } } */
