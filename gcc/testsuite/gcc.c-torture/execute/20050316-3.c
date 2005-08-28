extern void abort (void);

typedef int V2SI __attribute__ ((vector_size (8)));
typedef unsigned int V2USI __attribute__ ((vector_size (8)));
typedef short V2HI __attribute__ ((vector_size (4)));
typedef unsigned int V2UHI __attribute__ ((vector_size (4)));

V2USI
test1 (V2SI x)
{
  return (V2USI) (V2SI) (long long) x;
}

long long
test2 (V2SI x)
{
  return (long long) (V2USI) (V2SI) (long long) x;
}

int
main (void)
{
  if (sizeof (short) != 2 || sizeof (int) != 4 || sizeof (long long) != 8)
    return 0;

  union { V2SI x; int y[2]; V2USI z; long long l; } u;
  V2SI a = { -3, -3 };
  u.z = test1 (a);
  if (u.y[0] != -3 || u.y[1] != -3)
    abort ();

  u.l = test2 (a);
  if (u.y[0] != -3 || u.y[1] != -3)
    abort ();
  return 0;
}
