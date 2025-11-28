#include "tree-vect.h"

short c = 2;
short l = 6;
unsigned char m;
int k;
int a = -1;
unsigned long long t[2][2];

static void b( short c, int k, short l, unsigned m)
{
  for( signed x=0; x<2; x++)
    for( int ab=0; ab<k+2; ab++)
      a = ({
	   int ac = a;
	   int ad = ({  int ac = l ? m : t[x][0];
		     unsigned long long ad = c ? m : t[x][x];
		     ac < ad ? ac : ad;  });

	   ac < ad ? ac : ad;
	   });
}

int main()
{
  check_vect ();

  long long ag;
  b(c,k,l,m);
  ag = a;
  if (ag != -1)
    abort ();
}

