void abort (void);
void exit (int);

void f(long i)
{
  if ((signed char)i < 0 || (signed char)i == 0) 
    abort ();
  else
    exit (0);
}

int
main(void)
{
  f(0xffffff01);
}

