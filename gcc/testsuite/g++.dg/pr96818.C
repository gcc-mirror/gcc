// { dg-do compile }
// { dg-options "-O2" }

bool operatorY ();

struct l
{
  int m;
  int k;
  void n ();
    l ()
  {
    while (operatorY ())
      switch ((unsigned char) k)
	case 0:
	{
	  n ();
	  case 1:if (m)
	    ;
	}
  }
};

void
p ()
{
  l ();
}
