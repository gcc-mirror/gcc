extern void abort (void);

struct
{
  int b : 29;
} f;

void foo (short j)
{
  f.b = j;
}

int main()
{
  foo (-55);
  if (f.b != -55)
    abort ();
  return 0;
}
