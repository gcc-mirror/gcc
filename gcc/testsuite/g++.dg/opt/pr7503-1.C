// PR c++/7503
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort();

void test1a()
{
  int A = 4;
  int B = 4;

  (A > B ? A : B) = 1;
  if (A != 4 || B != 1)
    abort ();
}

void test1b()
{
  int A = 3;
  int B = 5;

  (A > B ? A : B) = 1;
  if (A != 3 || B != 1)
    abort ();
}

void test1c()
{
  int A = 5;
  int B = 3;

  (A > B ? A : B) = 1;
  if (A != 1 || B != 3)
    abort ();
}

void test2a()
{
  int A = 4;
  int B = 4;

  (A >= B ? A : B) = 1;
  if (A != 1 || B != 4)
    abort ();
}

void test2b()
{
  int A = 3;
  int B = 5;

  (A >= B ? A : B) = 1;
  if (A != 3 || B != 1)
    abort ();
}

void test2c()
{
  int A = 5;
  int B = 3;

  (A >= B ? A : B) = 1;
  if (A != 1 || B != 3)
    abort ();
}

void test3a()
{
  int A = 4;
  int B = 4;

  (A < B ? A : B) = 1;
  if (A != 4 || B != 1)
    abort ();
}

void test3b()
{
  int A = 3;
  int B = 5;

  (A < B ? A : B) = 1;
  if (A != 1 || B != 5)
    abort ();
}

void test3c()
{
  int A = 5;
  int B = 3;

  (A < B ? A : B) = 1;
  if (A != 5 || B != 1)
    abort ();
}

void test4a()
{
  int A = 4;
  int B = 4;

  (A <= B ? A : B) = 1;
  if (A != 1 || B != 4)
    abort ();
}

void test4b()
{
  int A = 3;
  int B = 5;

  (A <= B ? A : B) = 1;
  if (A != 1 || B != 5)
    abort ();
}

void test4c()
{
  int A = 5;
  int B = 3;

  (A <= B ? A : B) = 1;
  if (A != 5 || B != 1)
    abort ();
}


int main()
{
  test1a();
  test1b();
  test1c();

  test2a();
  test2b();
  test2c();

  test3a();
  test3b();
  test3c();

  test4a();
  test4b();
  test4c();

  return 0;
}

