void
test1 (int &ref)
{
#pragma acc kernels copy (ref)
  {
    ref = 10;
  }
}

void
test2 (int &ref)
{
  int b;
#pragma acc kernels copyout (b)
  {
    b = ref + 10;
  }

#pragma acc parallel copyout (b)
  {
    b = ref + 10;
  }

  ref = b;
}

int
main()
{
  int a = 0;
  int &ref_a = a;

  #pragma acc parallel copy (a, ref_a)
  {
    ref_a = 5;
  }

  return a;
}
