/* PR tree-optimization/57083 */
/* { dg-do run { target int32plus } } */

extern void abort (void);
short x = 1;
int y = 0;

int
main ()
{
  unsigned t = (0x7fff8001U - x) << (y == 0);
  if (t != 0xffff0000U)
    abort ();
  return 0;
}
