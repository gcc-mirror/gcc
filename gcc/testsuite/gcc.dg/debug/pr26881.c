/* PR debug/26881 */
/* { dg-do compile } */

int
foo ()
{
  if (0)
    {
      static union { } u;
      typedef char tt;
      static tt cccc[8];
      return cccc[0] == 0x01 && cccc[1] == 0x02;
    }
  return 0;
}
