extern void abort (void);
int i;
int main()
{
  int b = i != 0;
  int c = ~b;
  if (c != -1)
    abort ();
  return 0;
}
