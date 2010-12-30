// { dg-do run }

typedef enum { zero = 0, one = 1, two = 2, ENUM_MAX = 3 } my_enum;
my_enum e;
extern "C" void abort (void);
int __attribute__((noinline)) foo() { return 10; }
int main()
{
  int r;
  r = foo();
  if ((r < 0) || (r >= ENUM_MAX))
    return 0;
  e = (my_enum)r;
  abort ();
}
