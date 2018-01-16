// PR debug/83391
// { dg-do compile }
// { dg-options "-g" }
// { dg-additional-options "-mbranch-cost=1" { target { i?86-*-* x86_64-*-* mips*-*-* s390*-*-* avr*-*-* } } }

unsigned char a;
enum E { F, G, H } b;
int c, d;

void
foo ()
{
  int e;
  bool f;
  E g = b;
  while (1)
    {
      unsigned char h = a ? d : 0;
      switch (g)
	{
	case 0:
	  f = h <= 'Z' || h >= 'a' && h <= 'z';
	  break;
	case 1:
	  {
	    unsigned char i = h;
	    e = 0;
	  }
	  if (e || h)
	    g = H;
	  /* FALLTHRU */
	default:
	  c = 0;
	}
    }
}
