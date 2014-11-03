/* PR c/52769 */
/* { dg-do run } */
/* { dg-options "-O3" } */

typedef struct
{
  int should_be_zero;
  char s[6];
  int x;
} foo_t;

int
main (void)
{
  volatile foo_t foo = {
    .s = "123456",
    .x = 2
  };

  if (foo.should_be_zero != 0)
    __builtin_abort ();

  return 0;
}
