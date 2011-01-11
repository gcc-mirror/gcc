/* { dg-do compile } */
/* { dg-options "-O -fexceptions -fnon-call-exceptions -ftrapv" } */

void
foo ()
{
  int n = 0;
  while (1)
    {
      int i[n % 1];
      n++;
    }
}

