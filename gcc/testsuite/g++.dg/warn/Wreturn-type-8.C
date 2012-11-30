// PR c++/54046
// { dg-do compile }
// { dg-options "-O0 -Wall" }

void foo (void) __attribute__((noreturn));

struct A
{
  ~A () {}
};

bool
check1 (int x)
{
  A z;
  switch (x)
    {
    case 0:
      return false;
    default:
      throw "X";
      break;
    }
}

bool
check2 (int x)
{
  A z;
  switch (x)
    {
    case 0:
      return false;
    default:
      foo ();
      break;
    }
}

bool
check3 (int x)
{
  switch (x)
    {
    case 0:
      return false;
    default:
      throw "X";
      break;
    }
}

bool
check4 (int x)
{
  switch (x)
    {
    case 0:
      return false;
    default:
      foo ();
      break;
    }
}

bool
check5 (int x)
{
  A z;
  switch (x)
    {
    case 0:
      return false;
    default:
      throw "X";
    }
}

bool
check6 (int x)
{
  A z;
  switch (x)
    {
    case 0:
      return false;
    default:
      foo ();
    }
}
