/* { dg-do run } */
/* { dg-options "-g" } */

int *__attribute__((noipa)) consume (int *ptr) { return ptr; }

int
main (void)
{
  int x;
  int *volatile ptr = consume (&x);
  x = 0;
  x = 1;	/* { dg-final { gdb-test . "*ptr" "0" } } */
  return 0;	/* { dg-final { gdb-test . "*ptr" "1" } } */
}
