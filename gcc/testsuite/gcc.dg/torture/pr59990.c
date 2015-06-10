/* { dg-do run } */

extern void abort (void);

unsigned char value[4] = { 66, 9, 160, 255 };

int main (void)
{
  volatile float f;
  unsigned char a[4];

  __builtin_memcpy ((void *)&f, value, 4);
  __builtin_memcpy (a, (void *)&f, 4);
  if (a[2] != 160)
    abort ();

  return 0;
}
