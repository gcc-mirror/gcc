// { dg-do run  }
// { dg-options "-w" }
// Used to crash on the alpha with optimization.

extern "C" void abort (void);

struct Fix {
  unsigned short l;
};

static inline void f (int len)
{
  if (len > 65535)
    abort ();
}

struct Fix a = { 33 };

main()
{
  f (a.l);
}
