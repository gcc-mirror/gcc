#define int unsigned

struct foo
{
  int aa : 1;
  int a : 9;
  int c : 16;
  int d : 6;
};


int
foo (a, b)
     struct foo a;
{
  return a.d == 0;
}
