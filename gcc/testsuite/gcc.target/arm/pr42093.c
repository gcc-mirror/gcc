/* { dg-options "-mthumb -O2 -fno-reorder-blocks" }  */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-final { scan-assembler-not "tbb" } } */
/* { dg-final { scan-assembler-not "tbh" } } */

#include <stdlib.h>

int gbl;
int foo (int *buf, int n)
{
  int ctr = 0;
  int c;
  while (1)
    {
      c = buf[ctr++];
      switch (c)
        {
        case '\n':
          gbl++;
          break;

        case ' ': case '\t' : case '\f' : case '\r':
          break;

        case ';':
          do
            c = buf [ctr++];
          while (c != '\n' && c != -1);
          gbl++;
          break;

        case '/':
          {
            int prevc;
            c = buf [ctr++];
            if (c != '*')
              abort ();

            prevc = 0;
            while ((c = buf[ctr++]) && c != -1)
              {
                if (c == '\n')
                  gbl++;
              }
            break;
          }
        default:
          return c;
        }
    }
}
