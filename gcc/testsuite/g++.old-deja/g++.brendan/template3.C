// GROUPS passed templates
extern "C" void printf (char *, ...);
extern "C" void exit (int);

int count = 0;

void
die (int x)
{
  if (x != ++count)
    {
      printf ("FAIL\n");
      exit (1);
    }
}

class A {
  public:
    void   f() const { die (-1); }
};


template <class Item>
class B : public A {
  public:
  void f() const;
};

template <class Item>
inline void B<Item>::f() const { die (1); }

template <class Item>
class C : public A {
  public:
    void f() const { die (2); }
};


int main()
{
    B<int> b;
    C<int> c;

    b.f(); //- bugged, (A::f() called instead of B::f())
    c.f(); //- works fine (C::f() called)

    printf ("PASS\n");
    return 0;
}
