// PR middle-end/16693
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort();

char foo()
{
  return 0x10;
}

enum E { e = 0x0f };

int main()
{
  char c = (char)(E)(e & foo());
  if (c != 0)
    abort();
  return 0;
}

