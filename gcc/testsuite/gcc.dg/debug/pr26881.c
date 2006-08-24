/* { dg-do compile } */
/* { dg-options "-g -O0" } */
int
main (int argc, char **argv)
{
  if (0)
    {
      static union
      {
      }
      u;
      typedef char tt;
      static tt c[8];
      return c[0] == 0x01 && c[1] == 0x02;
    }
}
