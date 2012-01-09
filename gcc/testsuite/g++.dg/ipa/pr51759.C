/* { dg-do run } */
/* { dg-options "-O2"  } */

extern "C" void abort (void);
struct S
{
  void __attribute__((noinline)) set(unsigned val)
    {
      data = val;
      if (data != val)
        abort ();
    }
  int pad0;
  unsigned pad1 : 8;
  unsigned data : 24;
  int pad2;
};
int main()
{
  S s;
  s.pad2 = -1;
  s.set(0);
  if (s.pad2 != -1)
    abort ();
}

