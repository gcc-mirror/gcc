// { dg-do run }
// { dg-options "-O3" }

extern "C" void abort (void);

struct X {
    bool init;
    void foo() { if (!init) init = true; }
    void bar() { foo();                  }

};

typedef unsigned long long int uint64_t;
uint64_t mask1, mask2;

uint64_t calc() {
  return mask1 & mask2;
}

int main()
{
  mask1 = 0x00000000FFFFFFFFull;
  mask2 = 0x000000000000FFFFull;
  uint64_t value = calc();

  X().bar();

  if(value != calc())
    abort ();
  return 0;
}

