/* PR c/96549 */

long c = -1L;
long b = 0L;

int
main ()
{
  if (3L > (short) ((c ^= (b = 1L)) * 3L))
    return 0;
  __builtin_abort ();
}
