// PR c++/20763

typedef void *voidp;

struct S
{
  char a;
  voidp __attribute__ ((aligned (16))) b;
};

struct T
{
  char a;
  void *__attribute__ ((aligned (16))) b;
};

int f[sizeof (struct S) != sizeof (struct T) ? -1 : 1];
