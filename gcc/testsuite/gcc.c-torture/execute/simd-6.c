extern void abort (void);
extern int memcmp (const void *, const void *, __SIZE_TYPE__);

typedef unsigned char v8qi __attribute__((vector_size(8)));

v8qi foo(v8qi x, v8qi y)
{
  return x * y;
}

int main()
{
  v8qi a = { 1, 2, 3, 4, 5, 6, 7, 8 };
  v8qi b = { 3, 3, 3, 3, 3, 3, 3, 3 };
  v8qi c = { 3, 6, 9, 12, 15, 18, 21, 24 };
  v8qi r;

  r = foo (a, b);
  if (memcmp (&r, &c, 8) != 0)
    abort ();
  return 0;
}
