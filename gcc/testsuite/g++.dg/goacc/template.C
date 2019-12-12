#pragma acc routine
template <typename T> T
accDouble(int val)
{
  return val * 2;
}

template<typename T, int I> T
oacc_parallel_copy (T a)
{
  T b = 0;
  char w = 1;
  int x = 2;
  float y = 3;
  double z = 4;

#pragma acc parallel num_gangs (a) num_workers (a) vector_length (a) default (none) copyout (b) copyin (a)
#pragma acc loop gang worker vector
  for (int i = 0; i < 1; i++)
    b = a;

#pragma acc parallel num_gangs (a) copy (w, x, y, z)
#pragma acc loop
  for (int i = 0; i < 1; i++)
    {
      w = accDouble<char>(w);
      x = accDouble<int>(x);
      y = accDouble<float>(y);
      z = accDouble<double>(z);
    }

#pragma acc parallel num_gangs (a) if (1)
  {
#pragma acc loop independent collapse (2)
    for (int i = 0; i < a; i++)
      for (int j = 0; j < 5; j++)
	b = a;

#pragma acc loop auto tile (I, 3)
    for (int i = 0; i < a; i++)
      for (int j = 0; j < 5; j++)
	b = a;

#pragma acc loop seq
    for (int i = 0; i < a; i++)
      b = a;
  }

  T c;

#pragma acc parallel num_workers (10)
#pragma acc loop worker
  for (int i = 0; i < 1; i++)
    {
#pragma acc atomic capture
      c = b++;

#pragma atomic update
      c++;

#pragma acc atomic read
      b = a;

#pragma acc atomic write
      b = a;
    }

#pragma acc parallel reduction (+:c)
  c = 1;

#pragma acc data if (1) copy (b)
  {
#pragma acc parallel
    {
      b = a;
    }
  }

#pragma acc enter data copyin (b)
#pragma acc parallel present (b)
  {
    b = a;
  }

#pragma acc update host (b)
#pragma acc update self (b)
#pragma acc update device (b)
#pragma acc exit data delete (b)
#pragma acc exit data finalize copyout (b)
#pragma acc exit data delete (b) finalize

  return b;
}

template<typename T> T
oacc_kernels_copy (T a)
{
  T b = 0;
  T c = 0;
  char w = 1;
  int x = 2;
  float y = 3;
  double z = 4;

#pragma acc kernels num_gangs (a) num_workers (a) vector_length (a) default (none) copyout (b) copyin (a)
  for (int i = 0; i < 1; i++)
    b = a;

#pragma acc kernels copy (w, x, y, z)
  {
    w = accDouble<char>(w);
    x = accDouble<int>(x);
    y = accDouble<float>(y);
    z = accDouble<double>(z);
  }

#pragma acc kernels copyout (b) copyin (a)
  b = a;

#pragma acc kernels loop reduction (+:c)
  for (int i = 0; i < 10; i++)
    c = 1;

#pragma acc data if (1) copy (b)
  {
    #pragma acc kernels
    {
      b = a;
    }
  }

#pragma acc enter data copyin (b)
#pragma acc kernels present (b)
  {
    b = a;
  }

#pragma acc update host (b)
#pragma acc update self (b)
#pragma acc update device (b)
#pragma acc exit data delete (b)
#pragma acc exit data finalize copyout (b)
#pragma acc exit data delete (b) finalize

  return b;
}

int
main ()
{
  int b = oacc_parallel_copy<int, 4> (5);
  int c = oacc_kernels_copy<int> (5);

  return b + c;
}
