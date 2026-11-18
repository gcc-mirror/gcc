/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-additional-options "-fwrapv" } */

static volatile unsigned X = 0x80000000;
int g;

void __attribute__((noipa))
test (int y)
{
  if (y != -1073741824)
    __builtin_abort ();
}

int main ()
{ 
  unsigned x = X;
  g = (int)x / 2;
  test ((int)-x / 2);
  return 0;
} 
