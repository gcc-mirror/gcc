// { dg-options -fabi-version=11 }

extern void bar(int*);

void foo()
{
  {
    static int localVar = 0;
    bar(&localVar);
  }
  {
    static int localVar = 1;
    bar(&localVar);
  }
  {
    static int localVar = 2;
    bar(&localVar);
  }
  {
    static int localVar = 3;
    bar(&localVar);
  }
  {
    static int localVar = 4;
    bar(&localVar);
  }
  {
    static int localVar = 5;
    bar(&localVar);
  }
  {
    static int localVar = 6;
    bar(&localVar);
  }
  {
    static int localVar = 7;
    bar(&localVar);
  }
  {
    static int localVar = 8;
    bar(&localVar);
  }
  {
    static int localVar = 9;
    bar(&localVar);
  }
  {
    static int localVar = 10;
    bar(&localVar);
  }
  {
    static int localVar = 11;
    bar(&localVar);
  }
  {
    static int localVar = 12;
    bar(&localVar);
  }
}

// { dg-final { scan-assembler "_ZZ3foovE8localVar_9" } }
// { dg-final { scan-assembler "_ZZ3foovE8localVar__10_" } }
// { dg-final { scan-assembler "_ZZ3foovE8localVar__11_" } }
