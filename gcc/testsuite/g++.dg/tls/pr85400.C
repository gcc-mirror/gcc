// PR c++/85400
// Testcase by Brian Vandenberg <phantall@gmail.com>

// { dg-do link { target c++11 } }
// { dg-require-effective-target fpic }
// { dg-require-effective-target shared }
// { dg-require-effective-target tls }
// { dg-options "-shared -fPIC -O" }
// { dg-add-options tls }

struct Test
{
  int blah (int y)
  {
    thread_local int mything = 3;
    mything = y > 0 ? y : mything;
    return mything;
  }
};

int stuff (Test& test, int y)
{
  return test.blah(y);
}
