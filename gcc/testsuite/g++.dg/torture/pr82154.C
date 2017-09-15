// { dg-do compile }
// { dg-additional-options "-Wno-deprecated" }

namespace a {
int b;
class c
{
};
}
class g
{
public:
  g ();
};
using a::b;
class d
{
public:
  d ();
  void e ();
};
class f
{
  d
  i ()
  {
    static d j;
  }
  int *k () throw (a::c);
};


int *f::k () throw (a::c)
{
  static g h;
  i ();
  int l = 2;
  while (l)
    {
      --l;
      try
	{
	  operator new (b);
	}
      catch (a::c)
	{
	}
    }
  i ().e ();
}
