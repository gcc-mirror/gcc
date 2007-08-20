// PR c++/33025
// { dg-do run }
// { dg-options "-O2" }

typedef __SIZE_TYPE__ size_t;
inline void *operator new (size_t, void *p) throw () { return p; }
extern "C" void abort ();

int
main()
{
  const unsigned num = 10;
  unsigned *data = new unsigned[2 * num];
  unsigned *ptr = data;
  for (unsigned i = 0; i < 2 * num; ++i)
    (i % 2 == 0) ? new (ptr) unsigned (2) : new (ptr++) unsigned (1);
  if (ptr - data != num)
    abort ();
  return 0;
}
