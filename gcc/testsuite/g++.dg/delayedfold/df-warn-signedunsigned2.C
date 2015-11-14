/* { dg-do compile } */
/* { dg-options "-Wall -Werror" }  */

extern int fl;
extern int arr[];

#define MAK (fl < 0 ? 1 : (fl ? 2 : 0))

int foo (int sz)
{
  unsigned i;
  int r = 0;
  for (i = 0; i < MAK; i++)
    r += arr[i];
  return r;
}

