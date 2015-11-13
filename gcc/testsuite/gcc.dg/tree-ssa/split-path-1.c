/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-split-paths-details " } */

#include <stdio.h>
#include <stdlib.h>

#define RGBMAX 255

int
test()
{
  int i, Pels;
  unsigned char sum = 0;
  unsigned char xr, xg, xb;
  unsigned char xc, xm, xy, xk;
  unsigned char *ReadPtr, *EritePtr;

  ReadPtr = ( unsigned char *) malloc (sizeof (unsigned char) * 100);
  EritePtr = ( unsigned char *) malloc (sizeof (unsigned char) * 100);

  for (i = 0; i < 100;i++)
     {
       ReadPtr[i] = 100 - i;
     }

  for (i = 0; i < 100; i++)
     {
       xr = *ReadPtr++;
       xg = *ReadPtr++;
       xb = *ReadPtr++;

       xc = (unsigned char) (RGBMAX - xr);
       xm = (unsigned char) (RGBMAX - xg);
       xy = (unsigned char) (RGBMAX - xb);

       if (xc < xm)
         {
           xk = (unsigned char) (xc < xy ? xc : xy);
         }
       else
        {
          xk = (unsigned char) (xm < xy ? xm : xy);
        }

       xc = (unsigned char) (xc - xk);
       xm = (unsigned char) (xm - xk);
       xy = (unsigned char) (xy - xk);

       *EritePtr++ = xc;
       *EritePtr++ = xm;
       *EritePtr++ = xy;
       *EritePtr++ = xk;
       sum += *EritePtr;
    }
  return sum;
}

int
main()
{
  if (test() != 33)
    abort();

  return 0;
}

/* { dg-final { scan-tree-dump "Duplicating join block" "split-paths" } } */
