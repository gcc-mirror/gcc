int val;

int *ptr = &val;
float *ptr2 = (float *) &val;

__attribute__((optimize ("-fno-strict-aliasing")))
void
typepun (void)
{
  *ptr2=0;
}

int
main(void)
{
  *ptr=1;
  typepun ();
  if (*ptr)
    __builtin_abort ();
}

