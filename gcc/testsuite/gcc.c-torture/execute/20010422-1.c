void abort (void);
void exit (int);

unsigned int foo(unsigned int x)
{
  if (x < 5)
    x = 4;
  else
    x = 8;
  return x;
}

int main(void)
{
  if (foo (8) != 8)
    abort ();
  exit (0);
}
