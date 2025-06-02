/* { dg-do run } */
/* { dg-options "-std=gnu2y" } */

#define assert(e)  ((e) ? (void) 0 : __builtin_abort ())

void
inner_vla_noeval (void)
{
  int i;

  i = 3;
  static_assert (_Countof (struct {int x[i++];}[3]) == 3);
  assert (i == 3);
}

int
main (void)
{
  inner_vla_noeval ();
}
