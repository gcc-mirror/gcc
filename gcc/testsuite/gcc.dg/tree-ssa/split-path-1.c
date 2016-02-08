/* { dg-do run } */
/* { dg-options "-O2 -fsplit-paths -fdump-tree-split-paths-details --param max-jump-thread-duplication-stmts=20" } */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define RGBMAX 255 

unsigned char
test()
{
  int i, Pels;
  int sum = 0;
  unsigned char xr, xg, xb;
  unsigned char xc, xm, xy, xk = 0;
  unsigned char *ReadPtr, *EritePtr;

  ReadPtr = ( unsigned char *) malloc (sizeof (unsigned char) * 100);
  EritePtr = ( unsigned char *) malloc (sizeof (unsigned char) * 100);

  for (i = 0; i < 100;i++)
     {
       ReadPtr[i] = 100 - i;
     }

  for (i = 0; i < 24; i++)
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
       sum += *(--EritePtr);
       
    }
  return sum;
}

int
main()
{
  if (test() != 196)
    abort();

  return 0;
}

/* { dg-final { scan-tree-dump "Duplicating join block" "split-paths" } } */
