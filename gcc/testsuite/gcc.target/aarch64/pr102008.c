/* { dg-do compile } */
/* { dg-options "-O2" } */

struct Foo {  int a;  int b; };

int test(int side, const struct Foo *foo) {
  if (side == 1) return foo->a;
  return foo->b;
}

/* We want to if-convert the load, not the address.  */
/* { dg-final { scan-assembler-not "add" } } */
/* { dg-final { scan-assembler-times "csel" 1 } } */
