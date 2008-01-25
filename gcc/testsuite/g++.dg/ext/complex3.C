// PR c++/31780
// { dg-do run }
// { dg-options "" }

// Test that we can implicitly convert to _Complex, but that it's worse
// than a scalar arithmetic conversion.

extern "C" void exit (int);

int r = 0;

void f (_Complex int) { ++r; }
void f (double) { }

void g (_Complex int) { }

int main()
{
  f (1);
  g (1);

  return r;
}

void bar()
{
  r ? 0i : 0;
}
