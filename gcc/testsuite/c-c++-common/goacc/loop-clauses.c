int
main ()
{
  int i, j;

#pragma acc parallel firstprivate (j) private (i)
  {
#pragma acc loop seq
    for (i = 0; i < 10; i++)
      { }
  }

#pragma acc parallel default (none)
  {
#pragma acc loop auto private (j)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang(static:5)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang(static:*)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop vector
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop worker
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop auto
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop independent
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop seq
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang worker vector
    for (i = 0; i < 10; i++)
      { }
  }

#pragma acc kernels default (none)
  {
#pragma acc loop auto
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang (num:5)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang(static:5)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang(static:*)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop vector(length:10)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop worker(num:5)
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop auto
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop independent
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop seq
    for (i = 0; i < 10; i++)
      { }
#pragma acc loop gang worker vector
    for (i = 0; i < 10; i++)
      { }
  }

  return 0;
}
