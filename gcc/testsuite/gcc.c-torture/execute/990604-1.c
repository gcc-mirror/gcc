int b;
void f ()
{
  int i = 0;
  if (b == 0)
    do {
      b = i;
      i++;
    } while (i < 10);
}

int main ()
{
  f ();
  if (b != 9)
    abort ();
  return 0;
}

