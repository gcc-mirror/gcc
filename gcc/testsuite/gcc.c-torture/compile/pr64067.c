/* PR middle-end/64067 */

struct S { int s; };
int *const v[1] = { &((struct S) { .s = 42 }).s };

int *
foo (void)
{
  return v[0];
}
