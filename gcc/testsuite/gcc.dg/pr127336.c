/* PR middle-end/127336 */
/* { dg-do run { target sync_int_long } } */
/* { dg-options "-O2" } */

unsigned v = 9;

int
main ()
{
  if (__atomic_add_fetch (&v, v, __ATOMIC_RELAXED) != 18)
    __builtin_abort ();
}
