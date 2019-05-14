/* { dg-do run } */
/* { dg-options "-O2 " } */

extern void abort(void);

/* Distilled from fedora package veusz where we were not combining bitwise
 * AND's properly.  
 * The test adds lots fo conditions so that if the range is not accurate, 
 * it will shortcut the result and not get to the return 1 checks.  */

int
foo(int a, int i)
{
  if (i == 32 || i == 64 || i == 128)
    {
      int r = a & i;
      if (r < 32 || r > 128)
        return 0;
      if (r >32 && r < 64)
        return 0;
      if (r > 64 && r < 128)
        return 0;
      if (r == 32)
        return 1;
      if (r == 64)
        return 1;
      if (r == 128)
        return 1;
    }
  return 0;
}


int main()
{
  int x;
  int count = 0;
  for (x = 0; x < 255; x++)
    if (foo (255, x))
      count++;

  if (count != 3)
    abort();
}
