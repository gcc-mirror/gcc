// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

// excess errors test - XFAIL *-*-*

struct foo
{
  enum e
  {
    not				// ERROR - 
  };
  ~foo();
  void x (foo *&a, bool b = (unsigned char)0);
};

namespace N
{
  struct bar;

  template<class T>
  struct baz
  {
    baz(T *p);
  };

  typedef baz<bar> c;
}

struct z // crash test - XFAIL *-*-*
{
  int a;
};
