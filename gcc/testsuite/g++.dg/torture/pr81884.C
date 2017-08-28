/* { dg-do run } */

typedef unsigned long uint64_t;

struct value_t {
    uint64_t _count;
    value_t(uint64_t c) : _count(c) {}
};

struct X {
    value_t eventTime;
    uint64_t arr[0];
};

X* x;

__attribute__((noclone, noinline))
void initialize()
{
  x->arr[0] = 11;
  x->arr[1] = 12;
  x->eventTime = value_t(10);
  x->arr[2] = 13;
  x->arr[3] = 14;
}

int main()
{
  char buffer[sizeof(X) + sizeof(uint64_t)*4];
  x = (X*)buffer;
  x->eventTime = value_t(999);
  x->arr[0] = 1;
  x->arr[1] = 2;
  x->arr[2] = 3;
  x->arr[3] = 4;
  initialize();
  if (x->arr[0] != 11 || x->arr[1] != 12 || x->arr[2] != 13 || x->arr[3] != 14)
    __builtin_abort ();
}
