// Testcase for invocation of constructors/destructors in operator new[].
// { dg-do run }

#include <stdlib.h>

struct E {
  virtual ~E() { }
};

struct S {
  S();
  ~S();
};

static int count;
static int max;
static int throwAfter = -1;
static S *pS;

S::S()
{
  if (throwAfter >= 0 && count >= throwAfter)
    throw E();
  if (pS)
    {
      ++pS;
      if (this != pS)
	abort();
    }
  else
    pS = this;
  ++count;
  max = count;
}

S::~S()
{
  if (count > 1)
    {
      if (this != pS)
	abort();
      --pS;
    }
  else
    pS = 0;
  --count;
}

void __attribute__((noinline)) doit(int n)
{
  {
    S *s = new S[n];
    if (count != n)
      abort();
    if (pS != s + n - 1)
      abort();
    delete [] s;
    if (count != 0)
      abort();
  }
  throwAfter = 2;
  max = 0;
  try
    {
      new S[n];
      abort();
    }
  catch (E)
    {
      if (max != 2)
	abort();
    }
  throwAfter = -1;
}

int main()
{
  {
    S s;
    if (count != 1)
      abort();
    if (pS != &s)
      abort();
  }
  if (count != 0)
    abort();
  {
    S *s = new S;
    if (count != 1)
      abort();
    if (pS != s)
      abort();
    delete s;
    if (count != 0)
      abort();
  }
  {
    S *s = new S[1];
    if (count != 1)
      abort();
    if (pS != s)
      abort();
    delete [] s;
    if (count != 0)
      abort();
  }
  {
    S *s = new S[5];
    if (count != 5)
      abort();
    if (pS != s + 4)
      abort();
    delete [] s;
    if (count != 0)
      abort();
  }
  typedef S A[5];
  {
    S *s = new A;
    if (count != 5)
      abort();
    if (pS != s + 4)
      abort();
    delete [] s;
    if (count != 0)
      abort();
  }
  throwAfter = 2;
  max = 0;
  try
    {
      new S[5];
      abort();
    }
  catch (E)
    {
      if (max != 2)
	abort();
    }
  max = 0;
  try
    {
      new A;
      abort();
    }
  catch (E)
    {
      if (max != 2)
	abort();
    }
  throwAfter = -1;
  doit(5);
}
