// PR c++/7503
// { dg-do run }
// { dg-options "-O2 -Wno-deprecated" }

extern "C" void abort();

void test1a()
{
  int A = 4;
  int B = 4;

  A >?= B;
  if (A != 4 || B != 4)
    abort ();
}

void test1b()
{
  int A = 3;
  int B = 5;

  A >?= B;
  if (A != 5 || B != 5)
    abort ();
}

void test1c()
{
  int A = 5;
  int B = 3;

  A >?= B;
  if (A != 5 || B != 3)
    abort ();
}


void test2a()
{
  int A = 4;
  int B = 4;

  A <?= B;
  if (A != 4 || B != 4)
    abort ();
}

void test2b()
{
  int A = 3;
  int B = 5;

  A <?= B;
  if (A != 3 || B != 5)
    abort ();
}

void test2c()
{
  int A = 5;
  int B = 3;

  A <?= B;
  if (A != 3 || B != 3)
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

  return 0;
}

