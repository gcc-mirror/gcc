/* PR optimization/8634 */

extern void abort (void);

struct foo {
  const char a, b, c, d, e, f, g, h, i, j;
};

struct bar {
  const char a, b, c, d, e, f, g, h, i;
  char j;
};

int test1 ()
{
  struct foo X = { a : 'A', c : 'C', e : 'E', g : 'G', i : 'I' };
  char buffer[10];
  __builtin_memcpy (buffer, &X, 10);
  if (buffer[0] != 'A' || buffer[1] != '\0'
      || buffer[2] != 'C' || buffer[3] != '\0'
      || buffer[4] != 'E' || buffer[5] != '\0'
      || buffer[6] != 'G' || buffer[7] != '\0'
      || buffer[8] != 'I' || buffer[9] != '\0')
    abort ();
  return 0;
}

int test2 ()
{
  struct bar X = { a : 'A', c : 'C', e : 'E', g : 'G', i : 'I' };
  char buffer[10];
  __builtin_memcpy (buffer, &X, 10);
  if (buffer[0] != 'A' || buffer[1] != '\0'
      || buffer[2] != 'C' || buffer[3] != '\0'
      || buffer[4] != 'E' || buffer[5] != '\0'
      || buffer[6] != 'G' || buffer[7] != '\0'
      || buffer[8] != 'I' || buffer[9] != '\0')
    abort ();
  return 0;
}

int test3 ()
{
  struct foo X = { .b = 'B', .d = 'D', .f = 'F', .h = 'H' , .j = 'J' };
  char buffer[10];
  __builtin_memcpy (buffer, &X, 10);
  if (buffer[0] != '\0' || buffer[1] != 'B'
      || buffer[2] != '\0' || buffer[3] != 'D'
      || buffer[4] != '\0' || buffer[5] != 'F'
      || buffer[6] != '\0' || buffer[7] != 'H'
      || buffer[8] != '\0' || buffer[9] != 'J')
    abort ();
  return 0;
}

int test4 ()
{
  struct bar X = { .b = 'B', .d = 'D', .f = 'F', .h = 'H' , .j = 'J' };
  char buffer[10];
  __builtin_memcpy (buffer, &X, 10);
  if (buffer[0] != '\0' || buffer[1] != 'B'
      || buffer[2] != '\0' || buffer[3] != 'D'
      || buffer[4] != '\0' || buffer[5] != 'F'
      || buffer[6] != '\0' || buffer[7] != 'H'
      || buffer[8] != '\0' || buffer[9] != 'J')
    abort ();
  return 0;
}

int main ()
{
  test1 (); test2 (); test3 (); test4 ();
  return 0;
}
