// Used to crash on the alpha with optimization.
// Special g++ Options: -w

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
