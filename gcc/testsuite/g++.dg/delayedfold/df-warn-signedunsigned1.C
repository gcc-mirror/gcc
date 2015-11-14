/* { dg-do compile } */
/* { dg-options "-Wall -Werror" }  */

extern int fl;

#define MAK (fl < 0 ? 1 : (fl ? -1 : 0))

int foo (int sz)
{
  if (MAK) return 1;
  return 0;
}

