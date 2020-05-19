// { dg-do run }
// { dg-options "-O2 -fno-ipa-sra" }

typedef int int32_t __attribute__((mode (__SI__)));

volatile int global;
int __attribute__((noinline, noclone))
get_data (int i)
{
  global = i;
  return i;
}

typedef int array[32];

namespace {

char buf[512];

class A
{
public:
  int32_t field;
  char *s;

  A() : field(223344)
  {
    s = buf;
  }

  int __attribute__((noinline))
  foo (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j,
       int k, int l, int m, int n, int o, int p, int q, int r, int s, int t)
  {
    global = a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t;
    return global;
  }

  int __attribute__((noinline))
  bar()
  {
    int r = foo (get_data (1), get_data (1), get_data (1), get_data (1),
		 get_data (1), get_data (1), get_data (1), get_data (1),
		 get_data (1), get_data (1), get_data (1), get_data (1),
		 get_data (1), get_data (1), get_data (1), get_data (1),
		 get_data (1), get_data (1), get_data (1), get_data (1));

    if (field != 223344)
      __builtin_abort ();
    return 0;
  }
};

}

int main (int argc, char **argv)
{
  A a;
  int r = a.bar();
  r = a.bar ();
  if (a.field != 223344)
      __builtin_abort ();
  if (global != 20)
    __builtin_abort ();

  return r;
}
