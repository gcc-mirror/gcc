extern void abort (void);

int test(int n)
{
  struct s { char b[n]; } __attribute__((packed));
  n++;
  return sizeof(struct s);
}

int main()
{
  if (test(123) != 123)
    abort ();
  return 0;
}
