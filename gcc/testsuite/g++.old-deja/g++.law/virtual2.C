// { dg-do run  }
// GROUPS passed virtual-functions
// Not in g++ bugs snapshot
// From: grande@isi.edu (Jim Grande)
// Subject: g++ 2.3.3 (HPPA) virt class definition dumps core
// Date: 5 Mar 1993 22:51:14 -0500
// Message-ID: <23611@venera.isi.edu>

#include <stdio.h>

class doubleclass
{
	public:
		double	d;
		doubleclass(double x = 0)	{ d = x; };
};

class test
{
	doubleclass doublec;
};

class vderived : virtual public test
{
};

int main()
{
	vderived v;

	printf ("PASS\n");
}
