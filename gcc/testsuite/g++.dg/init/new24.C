// PR c++/33025
// { dg-do compile }
// { dg-options "-O2" }

typedef __SIZE_TYPE__ size_t;
inline void *operator new (size_t, void *p) throw () { return p; }
extern "C" void abort ();

int
main()
{
  const unsigned num = 10;
  unsigned *data = new unsigned[num];
  unsigned *ptr = new (data) unsigned (num);
  static unsigned data2[10];
  unsigned *ptr2 = new (&data2[0]) unsigned (10);
  return 0;
}
