/* { dg-do run } */
/* { dg-options "-g" } */

struct s { int i, j; };
struct s gs1, gs2 = { 3, 4 };

void __attribute__((noipa)) consume (void) {};

int
main (void)
{
  gs1.i = 1;
  gs1.j = 2;	/* { dg-final { gdb-test . "gs1.i" "1" } } */
  gs1 = gs2;	/* { dg-final { gdb-test . "gs1.j" "2" } } */
  consume ();	/* { dg-final { gdb-test . "gs1.i" "3" } } */
  return 0;	/* { dg-final { gdb-test . "gs1.j" "4" } } */
}
