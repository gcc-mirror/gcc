// { dg-do assemble  }
// { dg-options "-O1" }
// Origin: Jakub Jelinek <jakub@redhat.com>

struct foo {
  bool x;
  inline void a (unsigned char y);
  inline void b (void);
  virtual ~foo ();
};

foo::~foo ()
{
}

void foo::a (unsigned char y)
{
    x = ((y & 2) != 0);
}

void foo::b (void)
{
    a(0x07);
}
