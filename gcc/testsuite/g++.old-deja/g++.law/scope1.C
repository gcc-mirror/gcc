// { dg-do assemble  }
// GROUPS passed scoping
// scoping file
// From: kol@world.std.com (Nikolay Yatsenko)
// Date:     Fri, 16 Jul 1993 18:48:32 -0400
// Subject:  g++ gives wrong error for local structure
// Message-ID: <199307162248.AA05360@world.std.com>

int main(void)
{
  struct A{
  public:       int i;
    void set (int i)
      {A::i = i;}           // g++ gives wrong error
  };

  A a;
  a.set(17);
  return 0;
}
