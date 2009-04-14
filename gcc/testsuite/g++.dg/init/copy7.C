// PR c++/39480
// It isn't always safe to call memcpy with identical arguments.
// { dg-do run }

extern "C" void abort();
extern "C" void *
memcpy(void *dest, void *src, __SIZE_TYPE__ n)
{
  abort();
}

struct A
{
  double d[10];
};

struct B: public A
{
  char bc;
};

B b;

void f(B *a1, B* a2)
{
  *a1 = *a2;
}

int main()
{
  f(&b,&b);
}
