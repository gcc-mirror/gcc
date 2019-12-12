/* { dg-do run } */
/* { dg-options "-g" } */

struct s { int a, b, c, d; };

struct s gs1 = { 1, 2, 3, 4 };
struct s gs2 = { 5, 6, 7, 8 };

struct s *__attribute__((noipa)) consume (struct s *ptr) { return ptr; }

int
main (void)
{
  struct s x;
  struct s *volatile ptr = consume (&x);
  x = gs1;
  x = gs2;	/* { dg-final { gdb-test . "ptr->a" "1" } } */
  return 0;	/* { dg-final { gdb-test . "ptr->a" "5" } } */
}
