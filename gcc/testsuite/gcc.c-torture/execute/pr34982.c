extern void abort (void);

static void something();

int main()
{
  something(-1);
}

static void something(int i)
{
  if (i != -1)
    abort ();
}
