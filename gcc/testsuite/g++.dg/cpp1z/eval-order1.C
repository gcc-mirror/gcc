// P0145R2: Refining Expression Order for C++
// { dg-do run }
// { dg-options "-std=c++1z" }

extern "C" int printf (const char *, ...);
void sink(...) { }

int last = 0;
int f(int i)
{
  if (i < last)
    __builtin_abort ();
  last = i;
  return i;
}

int main()
{
  sink(f(1), f(2));
  sink(f(3), f(4), f(5));
}
