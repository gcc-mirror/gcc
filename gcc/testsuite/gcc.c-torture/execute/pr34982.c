/* { dg-additional-options "-std=gnu17" } */

extern void abort (void);

static void something();

int main()
{
  something(-1);
  return 0;
}

static void something(int i)
{
  if (i != -1)
    abort ();
}
