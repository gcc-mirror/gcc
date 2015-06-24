/* { dg-do run }  */
/* { dg-options "-O2" } */
void abort (void) __attribute__ ((noreturn));
float __attribute__((noinline))
f(float a, int b)
{
  return a - (((float)b / 0x7fffffff) * 100);
}

int
main (void)
{
  float a[] = { 100.0, 0.0, 0.0};
  int b[] = { 0x7fffffff, 0x7fffffff/100.0f, -0x7fffffff / 100.0f};
  float c[] = { 0.0, -1.0, 1.0 };
  int i;

  for (i = 0; i < (sizeof(a) / sizeof (float)); i++)
    if (f (a[i], b[i]) != c[i])
	abort ();

  return 0;
}
