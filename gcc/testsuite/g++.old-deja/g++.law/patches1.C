// { dg-do assemble  }
// GROUPS passed patches
// patches file
// From: david.binderman@pmsr.philips.co.uk
// Date:     Wed, 6 Oct 93 17:05:54 BST
// Subject:  Reno 1.2 bug fix
// Message-ID: <9310061605.AA04160@pmsr.philips.co.uk>

int type(float)       { return 1; }
int type(double)      { return 2; }
int type(long double) { return 3; }

extern "C" int printf( const char *, ...);

int main()
{
      int i = 0;
      if (type(0.0) != 2)
              ++i;
      if (i > 0)
	{ printf ("FAIL\n"); return 1; }
      else
	printf ("PASS\n");
}
