/* { dg-do run } */
/* { dg-options "-O1" } */

unsigned short v = 0x5678;
int i;
int j = 0;
int *ptr = &j;

int
func (void)
{
  for (i = 0; i < 1; ++i)
    {
      *ptr = -1;
      v = 0x1234;
    }
  return v;
}

int
main (void)
{
  func ();
  if (v != 0x1234)
    __builtin_abort ();
  return 0;
}
