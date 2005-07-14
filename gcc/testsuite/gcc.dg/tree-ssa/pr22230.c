/* { dg-do run } */
/* { dg-options "-O1 -ftree-vrp" } */

/* PR tree-optimization/22230

   The meet of the ranges in "i*i" was not computed correctly, leading
   gcc to believe that a was equal to 0 after the loop.  */

extern void abort (void) __attribute__((noreturn));

int main (void)
{
  long a, i;

  for (i = 0; i < 5; i++)
    a = i * i;
  if (a != 16)
    abort ();
  return 0;
}

