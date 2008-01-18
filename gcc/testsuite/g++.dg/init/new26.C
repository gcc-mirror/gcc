// PR c++/33407
// { dg-do run }
// { dg-options "-O2 -fstrict-aliasing" }

extern "C" void * malloc(__SIZE_TYPE__);
extern "C" void abort(void);

void *p;
void __attribute__((noinline)) init(void)
{
  p = malloc(4);
}

inline void *operator new(__SIZE_TYPE__)
{
  return p;
}

inline void operator delete (void*) {}

int * __attribute__((noinline)) doit(int n)
{
  float *q;
  int *r;

  for (int i=0; i<n; ++i)
  {
    q = new float;
    *q = 1.0;
    delete q;
    r = new int;
    *r = 1;
  }

  return r;
}

int main()
{
  init();
  if (*doit(1) != 1)
    abort();
  return 0;
}
