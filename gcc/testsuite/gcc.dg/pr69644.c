/* PR target/69644 */
/* { dg-do compile } */

int
main ()
{
  unsigned short x = 0x8000;
  if (!__sync_bool_compare_and_swap (&x, 0x8000, 0) || x)
    __builtin_abort ();
  return 0;
}
