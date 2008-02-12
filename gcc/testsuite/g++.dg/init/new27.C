// PR c++/34862
// { dg-do run }
// { dg-options "-O2" }

typedef __SIZE_TYPE__ size_t;
extern "C" void abort ();

struct T
{
  void *operator new (size_t, char *&);
  T () { i[0] = 1; i[1] = 2; }
  int i[2];
};

void *
T::operator new (size_t size, char *&p)
{
  void *o = (void *) p;
  p += size;
  return o;
}

T *
f (char *&x)
{
  return new (x) T ();
}

char buf[10 * sizeof (T)] __attribute__((aligned (__alignof (T))));

int
main ()
{
  char *p = buf;
  T *t = f (p);
  if (p != buf + sizeof (T))
    abort ();
  if (t->i[0] != 1 || t->i[1] != 2)
    abort ();
}
