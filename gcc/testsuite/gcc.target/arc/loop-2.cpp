/* { dg-options "-O2" } */
/* { dg-do assemble } */

/* This file fails to assemble if we forgot to increase the number of
   uses for loop's start and end labels.  */
int a, c, d;
int *b;
void fn1(int p1) {
  if (d == 5)
    for (int i; i < p1; ++i)
      if (c)
        b[i] = c;
      else
        int t = a = t;
  else
    for (int i; i < p1; ++i)
      b[i] = 0;
}
