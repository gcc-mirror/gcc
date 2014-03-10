// { dg-do run { target c++11 } }

extern "C" void abort ();
extern int ar[2];

int f()
{
  if (ar[0] != 42 || ar[1] != 0)
    abort ();
  return 1;
}

int i = f();

int ar[2] = { 42, i };

int main()
{
}
