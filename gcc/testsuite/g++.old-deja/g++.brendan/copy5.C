// GROUPS passed copy-ctors
/*
bad:
sibelius402> a.out
a=5  a.virtMember()=30
BaseClass::Increm --> {i=5, virtMember()=30}
a=7  a.virtMember()=30
b=7  b.virtMember()=30
BaseClass::Increm --> {i=7, virtMember()=999}
b=9  b.virtMember()=30
sibelius403> 

 good:

sibelius406> a.out
a=5  a.virtMember()=30
BaseClass::Increm --> {i=5, virtMember()=30}
a=7  a.virtMember()=30
b=7  b.virtMember()=30
BaseClass::Increm --> {i=7, virtMember()=30}
b=9  b.virtMember()=30
*/

extern "C" void printf (char *, ...);
extern "C" void exit (int);

void die () { printf ("FAIL\n"); exit (1); }

class BaseClass {

  friend int operator != (const BaseClass irv, int x);

  int i;

public:

  BaseClass( const BaseClass& ir ) : i(ir.i) {};
  BaseClass() : i(5) {};

  virtual int virtMember() { return( 999 ); };

  void Increm( int r );
};

void BaseClass::Increm( int r )
{
  if ((i == 5 && virtMember () == 30)
      || (i == 7 && virtMember () == 30))
    i += r;
  else
    die ();
};

class DerivedClass : public BaseClass {
public:
  int virtMember() { return( 30 ); };
};

int operator != (const BaseClass irv, int x) { return irv.i != x; }

int
main ()
{
    DerivedClass a;

    if (a != 5 || a.virtMember () != 30)
      die ();

    a.Increm(2);

    if (a != 7 || a.virtMember () != 30)
      die ();

    DerivedClass b = a;

    if (b != 7 || a.virtMember () != 30)
      die ();

    b.Increm(2);

    if (b != 9 || a.virtMember () != 30)
      die ();

    printf ("PASS\n");
}
